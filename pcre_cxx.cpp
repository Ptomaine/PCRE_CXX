/***************************************************************************
 *   Copyright (C) 2008, 2012 by Arlen Keshabyan                           *
 *   <arlen.albert@gmail.com>                                              *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#include "pcre_cxx.hpp"

namespace pcre_cxx_ns
{

inline auto pcre_cxx::reset() noexcept -> void
{
	_compilation_error.clear();
	_compilation_error_offset = 0;

	reset_pcre_objects();
}

inline auto pcre_cxx::reset_pcre_objects() noexcept -> void
{
	if (!_using_extern_regex)
        ::pcre_free(_extra_params);

	_extra_params = nullptr;

	if (!_using_extern_regex)
        ::pcre_free(_regexp);

	_regexp = nullptr;
}

auto pcre_cxx::has_digits(const std::string &subject, const int offset, const int digits_size) noexcept -> bool
{
    for (int idx { offset }, end { offset + digits_size }; idx < end; ++idx)
        if (subject[idx] < '0' || subject[idx] > '9')
            return false;

    return true;
}

inline auto pcre_cxx::need_prepare_replace(const std::string &replacement, const int digits_size) const noexcept -> bool
{
    const std::string::size_type position { replacement.find('$') };

	return (position != std::string::npos && position < (replacement.length() - 1) &&
			(replacement[position + 1] == '$' || pcre_cxx::has_digits(replacement, position + 1, digits_size)));
}

auto pcre_cxx::get_group_index(const std::string &subject, const int offset, const int digits_size) noexcept -> int
{
	 int result{ 0 }, end{ offset }, mul{ 1 };

     for (int idx(offset + digits_size - 1); idx >= end; result += (subject[idx] - '0') * mul, mul *= 10, --idx) ;

     return result;
}

inline auto pcre_cxx::prepare_replace(const pcre_cxx::pcre_cxx_context &context, const std::string &replacement, const int digits_size) const -> const std::string
{
	const std::string &subject{ *context._subject };
	std::string result{ replacement };
	std::string::size_type position{ 0 }, group_index{ 0 };
	int group_offset{ 0 }, group_length{ 0 };

	while ((position = result.find('$', position)) != std::string::npos)
	{
		if (position < result.length() - digits_size)
		{
			if (pcre_cxx::has_digits(result, position + 1, digits_size))
			{
				if (digits_size == 1)
					group_index = result[position + 1] - '0';
				else
					group_index = pcre_cxx::get_group_index(result, position + 1, digits_size);

				if (context._captures_size > 0 && group_index < static_cast<std::string::size_type>(context._captures_size))
				{
					group_offset = 2 * group_index;
					group_length = context._result_vector[group_offset + 1] - context._result_vector[group_offset];
					result.replace(position, digits_size + 1, (subject.substr(context._result_vector[group_offset], group_length)));

					position += group_length - 1;
				}
			}
			else
				if (result[position + 1] == '$')
					result.replace(position, 2, "$");

			position++;
		}
	}

	return result;
}

inline auto pcre_cxx::extract_captures(const pcre_cxx::pcre_cxx_context &context, groups_t &groups) const -> const groups_t&
{
	const std::string &subject{ *context._subject };

	if (context._captures_size < 1)
		return groups;

	groups.reserve(context._captures_size);

	const int double_size{ 2 * context._captures_size };

	for (int offset{ 0 }; offset < double_size; offset += 2)
		if (context._result_vector[offset] >= 0)
			groups.push_back(subject.substr(context._result_vector[offset], context._result_vector[offset + 1] - context._result_vector[offset]));
		else
			groups.push_back(std::string());

	return groups;
}

inline auto pcre_cxx::extract_captures(const pcre_cxx::pcre_cxx_context &context, raw_groups_t &raw_groups) const -> const raw_groups_t&
{
	if (context._captures_size < 1)
		return raw_groups;

	raw_groups.reserve(context._captures_size);

	const int double_size{ 2 * context._captures_size };

	for (int offset{ 0 }; offset < double_size; offset += 2)
		raw_groups.push_back(raw_group_t(context._result_vector[offset], context._result_vector[offset + 1]));

	return raw_groups;
}

inline auto pcre_cxx::execute(pcre_cxx::pcre_cxx_context &context, int options) noexcept -> const pcre_cxx&
{
	const std::string &subject{ *context._subject };

	if (_use_dfa)
	{
		int dfa_workspace[pcre_cxx_context::max_captures_size];

		context._captures_size = ::pcre_dfa_exec(_regexp, _extra_params, subject.c_str(), subject.length(), context._tailed_subject_offset, options, context._result_vector, pcre_cxx_context::max_captures_size, dfa_workspace, pcre_cxx_context::max_captures_size);
	}
	else
		context._captures_size = ::pcre_exec(_regexp, _extra_params, subject.c_str(), subject.length(), context._tailed_subject_offset, options, context._result_vector, pcre_cxx_context::max_captures_size);

	if (context._captures_size > 0)
		context._tailed_subject_offset = context._result_vector[1];
	else
	{
		context._execution_error_code = context._captures_size;
		context._captures_size = 0;
	}

	return *this;
}

pcre_cxx::pcre_cxx() noexcept
{
}

pcre_cxx::pcre_cxx(const std::string &pattern, int options, bool study) noexcept
{
	compile(pattern, options, study);
}

pcre_cxx::pcre_cxx(pcre *const regexp, pcre_extra *const regexp_extra) noexcept :
	_extra_params{ regexp_extra },
	_regexp{ regexp },
	_using_extern_regex{ true }
{
}

pcre_cxx::~pcre_cxx()
{
	reset_pcre_objects();
}

auto pcre_cxx::compile(const std::string &pattern, int options, bool study) noexcept -> bool
{
	reset();

	const char *error{ nullptr };

	_regexp = ::pcre_compile(pattern.c_str(), options, &error, &_compilation_error_offset, nullptr);
	_compilation_error = error ? error : "";

	if (_regexp && study)
	{
		_extra_params = ::pcre_study(_regexp, 0, &error);
		_compilation_error = error ? error : "";
	}

	return (!!_regexp);
}

auto pcre_cxx::match_next(pcre_cxx::pcre_cxx_context &context, int options) -> int
{
	return execute(context, options).captures_size(context);
}

auto pcre_cxx::current_raw_captures(const pcre_cxx::pcre_cxx_context &context) const -> const raw_groups_t
{
	raw_groups_t raw_groups;

	return extract_captures(context, raw_groups);
}

auto pcre_cxx::current_captures(const pcre_cxx::pcre_cxx_context &context) const -> const groups_t
{
	groups_t groups;

	return extract_captures(context, groups);
}

auto pcre_cxx::capture_next(pcre_cxx::pcre_cxx_context &context, int options) -> const groups_t
{
	groups_t groups;

	return execute(context, options).extract_captures(context, groups);
}

auto pcre_cxx::capture_all(pcre_cxx::pcre_cxx_context &context, int options) -> const matches_t
{
	matches_t matches;

	while (execute(context, options).captures_size(context) > 0)
	{
		groups_t groups;
		matches.push_back(extract_captures(context, groups));
	}

	return matches;
}

auto pcre_cxx::split_next(pcre_cxx::pcre_cxx_context &context, int options) -> const groups_t
{
	const std::string &subject{ *context._subject };
	bool captured{ execute(context, options).captures_size(context) > 0 };

	groups_t result;

	result.reserve(2);

	if (captured)
	{
		result.push_back(subject.substr(context._tailed_subject_offset[1], context._result_vector[0] - context._tailed_subject_offset[1]));
		result.push_back(subject.substr(context._result_vector[0], context._result_vector[1] - context._result_vector[0]));
	}
	else
	{
		size_t length{ subject.length() };

		if (context._tailed_subject_offset[0] < static_cast<int>(length))
		{
			result.push_back(subject.substr(context._tailed_subject_offset[0]));
			result.push_back({});

			context._tailed_subject_offset = length + 1;
			captured = true;
		}
	}

	return result;
}

auto pcre_cxx::split_all(pcre_cxx::pcre_cxx_context &context, int options) -> const matches_t
{
	matches_t matches;
	groups_t groups;

	while ((groups = split_next(context, options)).size() > 0)
		matches.push_back(std::move(groups));

	return matches;
}

auto pcre_cxx::name_index(const std::string &name) const noexcept -> int
{
	return ::pcre_get_stringnumber(_regexp, name.c_str());
}

auto pcre_cxx::name_indices(const std::string &name) const -> const indices_t
{
	indices_t result;

	char *first_entry{ nullptr }, *last_entry{ nullptr };
	int entry_length{ ::pcre_get_stringtable_entries(_regexp, name.c_str(), &first_entry, &last_entry) };

	if (entry_length > 0)
	{
		result.reserve((last_entry - first_entry) / entry_length);

		for (; first_entry <= last_entry; first_entry += entry_length)
			result.push_back(int((first_entry[0] << 8) + first_entry[1]));
	}

	return result;
}

auto pcre_cxx::replace(pcre_cxx::pcre_cxx_context &context, const std::string &replacement, const int replacement_digits_size, const int options, const int group_index, bool replace_all) -> const std::string&
{
    int group_length{ 0 };
	const int group_offset{ 2 * group_index };
	const bool call_prepare_replace{ need_prepare_replace(replacement, replacement_digits_size) };
	std::string full_replacement{ replacement }, result_string{ *context._subject };
	pcre_cxx::pcre_cxx_context _context{ result_string };

	while (execute(_context, options).captures_size(_context) > 0)
	{
		if (group_index >= _context._captures_size)
			continue;

		if (call_prepare_replace)
			full_replacement = prepare_replace(_context, replacement, replacement_digits_size);

		group_length = _context._result_vector[group_offset + 1] - _context._result_vector[group_offset];

		result_string.replace(_context._result_vector[group_offset], group_length, full_replacement);

		_context._tailed_subject_offset = _context._result_vector[group_offset + 1] + (full_replacement.length() - group_length);

		if (!replace_all) break;
	}

	(~context).set_subject_copy(result_string);

	return *(~context)._subject;
}

auto pcre_cxx::set_current_subject_offset(pcre_cxx::pcre_cxx_context &context, int offset) noexcept -> pcre_cxx&
{
	~context._tailed_subject_offset = offset;
	context._tailed_subject_offset = offset;

	return *this;
}

auto pcre_cxx::get_current_subject_offset(const pcre_cxx::pcre_cxx_context &context) const noexcept -> int
{
	return context._tailed_subject_offset;
}

auto pcre_cxx::use_dfa(bool use) noexcept -> pcre_cxx&
{
	_use_dfa = use;

	return *this;
}

auto pcre_cxx::has_error(const pcre_cxx::pcre_cxx_context &context) const noexcept -> bool
{
    return (!_regexp || !_compilation_error.empty() || context._execution_error_code);
}

auto pcre_cxx::compilation_error() const noexcept -> const std::string
{
	return _compilation_error;
}

auto pcre_cxx::compilation_error_offset() const noexcept -> int
{
	return _compilation_error_offset;
}

auto pcre_cxx::execution_error_code(const pcre_cxx::pcre_cxx_context &context) const noexcept -> int
{
	return context._execution_error_code;
}

auto pcre_cxx::extra_options() const noexcept -> pcre_extra&
{
	return *_extra_params;
}

auto pcre_cxx::captures_size(const pcre_cxx::pcre_cxx_context &context) const noexcept -> int
{
	return context._captures_size;
}

pcre_cxx::operator const pcre *() const noexcept
{
	return _regexp;
}

pcre_cxx::operator const pcre_extra *() const noexcept
{
	return _extra_params;
}

auto pcre_cxx::operator [](const std::string &name) const noexcept -> int
{
	return name_index(name);
}

auto pcre_cxx::get_compiled_data(byte_buffer_t &compiled_pattern_buffer, byte_buffer_t &compiled_study_info) const -> void
{
    compiled_pattern_buffer.clear(), compiled_study_info.clear();

    if (_regexp)
    {
        int size{ 0 }, rc{ 0 };
        rc = pcre_fullinfo(_regexp, nullptr, PCRE_INFO_SIZE, &size);

        if (rc >= 0)
        {
            std::copy(reinterpret_cast<unsigned char*>(_regexp), reinterpret_cast<unsigned char*>(_regexp) + size, (compiled_pattern_buffer.resize(size), &compiled_pattern_buffer.front()));

            if (_extra_params)
            {
                size = 0, rc = 0;
                rc = pcre_fullinfo(_regexp, _extra_params, PCRE_INFO_STUDYSIZE, &size);

                if (rc >= 0)
                    std::copy(reinterpret_cast<unsigned char*>(_extra_params), reinterpret_cast<unsigned char*>(_extra_params) + size, (compiled_study_info.resize(size), &compiled_study_info.front()));
            }
        }
    }
}

auto pcre_cxx::set_compiled_data(const byte_buffer_t &compiled_pattern_buffer, const byte_buffer_t &compiled_study_info) -> void
{
    if (compiled_pattern_buffer.size())
    {
        reset_pcre_objects();

        _regexp = reinterpret_cast<pcre*>(::pcre_malloc(compiled_pattern_buffer.size()));
        std::copy(&compiled_pattern_buffer.front(), &compiled_pattern_buffer.back() + 1, reinterpret_cast<unsigned char*>(_regexp));

        if (compiled_study_info.size())
        {
            _extra_params = reinterpret_cast<pcre_extra*>(::pcre_malloc(compiled_study_info.size()));
            std::copy(&compiled_study_info.front(), &compiled_study_info.back() + 1, reinterpret_cast<unsigned char*>(_extra_params));
            _extra_params->study_data = _extra_params + sizeof(pcre_extra);
        }
    }
}

}
