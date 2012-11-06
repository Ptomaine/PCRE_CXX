#pragma once

/***************************************************************************
 *   Copyright (C) 2008, 2012 by Arlen Albert Keshabyan                    *
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

#ifndef PCRE_STATIC
	#define PCRE_STATIC
#endif

#if (defined(__GNUC__) && (__GNUC__ * 10000 + __GNUC_MINOR__ * 100) >= 40600) \
			|| (defined(_MSC_VER) && _MSC_VER >= 1600)
	#define CPP2011_SUPPORTED
#endif

#include "pcre/config.h"
#include "pcre/pcre.h"

#include <string>
#include <vector>
#include <deque>

#ifndef CPP2011_SUPPORTED
	#define nullptr 0
#endif

/// The header file to handle regular expressions
/** @file pcre_cxx.hpp
 * Yet another C++ library to handle regular expressions
 */

/** @namespace pcre_cxx_ns
 * A namespace that contains classes to handle regular expressions
 */
namespace pcre_cxx_ns
{
/// A type to hold a captured groups
/** @typedef groups_t
 * Groups are captured in a vector of strings for a single match
 */
typedef std::vector<std::string> groups_t;

/// A type to hold all matches
/** @typedef matches_t
 * All matches with captured groups are stored in a vector of ::groups_t
 */
typedef std::vector<groups_t> matches_t;

typedef std::vector<int> indices_t;

typedef std::pair<int, int> raw_group_t;

/// A type to hold a captured groups in a raw format
/** @typedef raw_groups_t
 * Groups are captured in a vector of pairs (of int offset positions and sizes) for a single match
 */
typedef std::vector<raw_group_t> raw_groups_t;

typedef std::vector<unsigned char> byte_buffer_t;

/// The tailed class can store several values
/** @class tailed
 * This class can remember several values in the LIFO way.
 * The number of values it's able to store is to set while instantiation.
 */
template<typename type, const int tail_limit = 2>
class tailed
{
private:

	std::deque<type> _values;

	inline auto push() -> type&
	{
		_values.push_front(_values.front());
		_values.pop_back();

		return _values.front();
	}

public:

    typedef type value_type;

	constexpr tailed() :
	    _values(tail_limit)
	{
	}

	tailed(const value_type &value) :
	    _values(tail_limit)
	{
		_values.front() = value;
	}

	tailed(const tailed &copy) :
	    _values(copy._values)
	{
	}

	auto operator = (const value_type &value) -> tailed&
	{
		push() = value;

		return *this;
	}

	auto operator = (const tailed &copy) -> tailed&
	{
		_values = copy._values;

		return *this;
	}

	tailed(tailed &&copy)  :
	    _values(std::move(copy._values))
	{
	}

	auto operator = (tailed &&copy) -> tailed&
	{
		_values = std::move(copy._values);

		return *this;
	}

	auto operator () () -> value_type&
	{
		return _values.front();
	}

	auto operator * () -> value_type&
	{
		return push();
	}

	auto operator += (const value_type &value) -> tailed&
	{
		push() += value;

		return *this;
	}

	auto operator -= (const value_type &value) -> tailed&
	{
		push() -= value;

		return *this;
	}

	auto operator *= (const value_type &value) -> tailed&
	{
		push() *= value;

		return *this;
	}

	auto operator /= (const value_type &value) -> tailed&
	{
		push() /= value;

		return *this;
	}

	auto operator ++ () -> tailed&
	{
		push()++;

		return *this;
	}

	auto operator ++ (int) -> tailed
	{
		tailed temp(*this);

		push()++;

		return temp;
	}

	auto operator -- () -> tailed&
	{
		push()--;

		return *this;
	}

	auto operator -- (int) -> tailed
	{
		tailed temp(*this);

		push()--;

		return temp;
	}

	operator value_type& ()
	{
		return _values.front();
	}

	operator value_type() const
	{
		return _values.front();
	}

	auto operator [](const int index) -> value_type&
	{
		return _values[index];
	}

	auto operator [](const int index) const -> value_type
	{
		return _values[index];
	}

	auto operator ~() -> tailed&
	{
		std::fill(_values.begin(), _values.end(), value_type());

		return *this;
	}
};

/// The main regular expression class
/** @class pcre_cxx
 * This class has almost everything to execute a regular expression over a subject.
 * The class is a simple wrapper class around the famous PCRE library written in C.
 */
class pcre_cxx
{
public:
    /// The regular expression's context class
    /** @class pcre_cxx_context
     * This class is to be used as a context object for the pcre_cxx class.
     * The main purpose of the class is to support multi-threaded usage.
     * You may use a single pcre_cxx class instance (with a particular pattern) for several threads but
     * you have to use a separate contexts for each thread.
     * This class holds a reference to a subject or, sometimes, a full copy of it (see @ref pcre_cxx_context(const std::string &subject))
     * and tracks subject info in a thread-safe way.
     */
	class pcre_cxx_context
	{
	    /** @cond */
	private:
		static const unsigned max_captures_size = (MAX_NAME_COUNT + 1) * 3;

		int _result_vector[max_captures_size]; /*!  */
		int _captures_size{ 0 };
		int _execution_error_code{ 0 };
		tailed<int, 2> _tailed_subject_offset{};
		std::string _subject_copy{};
		const std::string *_subject{ &_subject_copy };

		friend class pcre_cxx;

		pcre_cxx_context(const pcre_cxx_context &) = delete;
		pcre_cxx_context &operator =(const pcre_cxx_context &) = delete;
		/** @endcond */

	public:
		/// The default constructor.
		/**
         * The default and preferred constructor.
         * Use this constructor to create a context.
         * You can change a subject runtime by using @ref set_subject function
         * or using the overloaded parenthesis operator.
         * So, try to use this default constructor only.
		 */
		pcre_cxx_context()
        {
        }

		/// The mutable string constructor.
		/**
		 * Use this constructor to reference a mutable string class.
		 * @see set_subject(const std::string &subject)
		 */
		explicit pcre_cxx_context(std::string &subject) :
		    _subject(&subject)
        {
        }

		/// The temporary string class constructor.
		/**
		 * Use this constructor only and only when using the implicit string constructor
		 * that creates a temporary string class to a constant char array in cases like this:
		 * @code
		 * pcre_cxx_context reg_context("This constant char array subject is going to be converted to \
		 * a constant temporary string class object implicitly");
		 * @endcode
		 * This constructor creates a full copy of a subject since a constant string object instance, provided as a constructor parameter,
		 * is going to be destructed upon a constructor exit.
		 * That's why it is recommended to use the default constructor (or the constructor with a mutable string)
		 * and to use the overloaded parenthesis operator afterwards to change a subject safely even with a constant temporary string object.
		 * Any other constructors and @ref set_subject function never create a full copy of a subject but use a reference to an object instead
		 * since the subject string may be very big in size and thus very expensive to copy.
		 */
		explicit pcre_cxx_context(const std::string &subject) :
		    _subject_copy(subject)
        {
        }

		/// Resets the object
		/**
         * This function resets the internal state of a context object to its initial state
         * and preserves subject string.
         * This function is going to be used in very rear cases since @ref set_subject function
         * rewinds object state automatically.
         * You may use it if you do several manipulations upon the same subject. For example,
         * you may wish to capture all matches and then replace some parts of the same subject.
         * In such scenarios, before calling the pcre_cxx::replace() function you have to rewind
         * context to make replace work properly. Also, you can use a shortcut like this:
         * regex.replace(~reg_context, ...);
		 */
		auto rewind() -> pcre_cxx_context& { return _captures_size = 0, _execution_error_code = 0, ~_tailed_subject_offset, *this; }

		/// Shortcut for the @ref rewind() function
		auto operator ~() -> pcre_cxx_context& { return rewind(); }

		/// Sets a new subject reference to work with
		/**
		 * This function sets a new subject to the current context
		 * and rewinds it making it ready to work with.
		 */
		auto set_subject(const std::string &subject) -> pcre_cxx_context& { return _subject = &subject, rewind(); }
		auto set_subject_copy(const std::string &subject) -> pcre_cxx_context& { return _subject_copy = subject, _subject = &_subject_copy, rewind(); }

		/// Shortcut for the set_subject(const std::string &subject) function
		auto operator ()(const std::string &subject) -> pcre_cxx_context& { return set_subject(subject); }
		auto operator [](const std::string &subject) -> pcre_cxx_context& { return set_subject_copy(subject); }

		auto get_subject() const -> const std::string& { return *_subject; }
	};

    /** @cond */
protected:

	pcre_extra *_extra_params{ nullptr };
	pcre *_regexp{ nullptr };
	bool _use_dfa{ false };
	std::string _compilation_error{};
	int _compilation_error_offset{ 0 };
	bool _using_extern_regex{ false };

	inline auto reset() noexcept -> void;
	inline auto reset_pcre_objects() noexcept -> void;
	inline auto need_prepare_replace(const std::string &replacement, const int digits_size) const noexcept-> bool;
	inline auto prepare_replace(const pcre_cxx::pcre_cxx_context &context, const std::string &replacement, const int digits_size) const -> const std::string;
	inline auto extract_captures(const pcre_cxx::pcre_cxx_context &context, groups_t &groups) const -> const groups_t&;
	inline auto extract_captures(const pcre_cxx::pcre_cxx_context &context, raw_groups_t &raw_groups) const -> const raw_groups_t&;
	inline auto execute(pcre_cxx::pcre_cxx_context &context, int options) noexcept -> const pcre_cxx&;
	static auto has_digits(const std::string &subject, const int offset, const int digits_size) noexcept -> bool;

	pcre_cxx(const pcre_cxx &) = delete;
	pcre_cxx &operator = (const pcre_cxx&) = delete;
    /** @endcond */

public:
    /** @cond */
    static auto get_group_index(const std::string &subject, const int offset, const int digits_size) noexcept -> int;
    /** @endcond */

    /// A default constructor.
    /**
     * A default constructor that initializes an object.
     * You must call compile() member function before any other matching
     * functions of the object in order to make it execute properly
     *
     * @see pcre_cxx(const std::string &pattern, int options, bool study)
     */
	pcre_cxx() noexcept;

    /// A parametrized constructor.
    /**
     * The preferred constructor that initializes an object, compiles and studies
     * a regular expression if required.
     *
     * @param pattern is a regular expression to compile and optionally study
     * @param options are options to use to compile a regular expression. They are exactly the same as used for pcre_compile library function
     * @param study is used to indicate whether the pcre_compile library function has to study a pattern or has not
     *
     * It calls @ref compile(const std::string &pattern, int options, bool study) function internally.
     * Thus, letting you prepare (initialize, compile and study) a regular expression pattern in one step.
     * Call @ref has_error() right after the instantiation just to be sure the instance is created and complied OK.
     *
     * A typical use:
     * @code
     *
     *    pcre_cxx_context reg_context;
     *    pcre_cxx reg_ex("\\w+");
     *
     *    if (reg_ex.has_error())
     *       throw reg_ex.compilation_error();
     *
     *    matches_t matches(reg_ex.capture_all(reg_context("This subject is going to be broken down into words")));
     *
     *    for (matches_t::iterator it = matches.begin(), end = matches.end(); it != end; ++it)
     *       for (groups_t git = (*it).begin(), gend = (*it).end(); git != gend; ++git)
     *          std::cout << "'" << (*git) << "'" << std::endl;
     *
     * @endcode
     * @see compile(const std::string &pattern, int options, bool study)
     */
    pcre_cxx(const std::string &pattern, int options = 0, bool study = true) noexcept;

    pcre_cxx(pcre *const regexp, pcre_extra *const regexp_extra = nullptr) noexcept;

    /// A virtual destructor
    /**
     * A class destructor
     */
	virtual ~pcre_cxx();

    /// A function to compile a regular expression.
    /**
     * This function compiles and studies a regular expression thus preparing it
     * to capture, to split or to replace substrings from a subject.
     *
     * @param pattern is a regular expression to compile and optionally study
     * @param options are options to use to compile a regular expression. They are exactly the same as used for pcre_compile library function
     * @param study is used to indicate whether the pcre_compile library function has to study a pattern or has not
     * @return This function returns true on successful compilation or false otherwise
     * @see pcre_cxx(const std::string &pattern, int options, bool study)
     */
    auto compile(const std::string &pattern, int options = 0, bool study = true) noexcept -> bool;

    ///Finds a next match against a subject
    /**
     * Each call to this function continues searching the same subject for a next match.
     * It starts at the beginning of a subject and continues searching on the next call from an offset stored from the last call.
     * Supply the same subject to the function each time you want to find a next match within the same subject.
     * Avoid supplying a different subject on the same pattern without rewinding the object because the behavior is undefined in the case.
     *
     * If you want to extract a current set of captured strings you may call the @ref current_captures() function
     * or call the @ref current_raw_captures() to extract offsets and sizes of current captured groups.
     *
     * See pcre_exec PCRE library function for more information.
     *
     * @param context is the context object with a subject string where you want to find a match with a regular expression pattern
     * @param options is a set of options. This set is the same as for the pcre_exec library function
     * @return The function returns a number of captures a pattern has matched against a current subject
     * @see pcre_cxx_context::rewind()
     */
	auto match_next(pcre_cxx::pcre_cxx_context &context, int options = 0) -> int;


	auto current_raw_captures(const pcre_cxx::pcre_cxx_context &context) const -> const raw_groups_t;
	auto current_captures(const pcre_cxx::pcre_cxx_context &context) const -> const groups_t;
	auto capture_next(pcre_cxx::pcre_cxx_context &context, int options = 0) -> const groups_t;
	auto capture_all(pcre_cxx::pcre_cxx_context &context, int options = 0) -> const matches_t;
	auto split_next(pcre_cxx::pcre_cxx_context &context, int options = 0) -> const groups_t;
	auto split_all(pcre_cxx::pcre_cxx_context &context, int options = 0) -> const matches_t;
	auto name_index(const std::string &name) const noexcept -> int;
	auto name_indices(const std::string &name) const -> const indices_t;
	auto replace(pcre_cxx::pcre_cxx_context &context, const std::string &replacement, const int replacement_digits_size = 1, const int options = 0, const int group_index = 0, bool replace_all = true) -> const std::string&;

	auto set_current_subject_offset(pcre_cxx::pcre_cxx_context &context, int offset = 0) noexcept -> pcre_cxx&;
	auto get_current_subject_offset(const pcre_cxx::pcre_cxx_context &context) const noexcept -> int;
	auto use_dfa(bool use = true) noexcept -> pcre_cxx&;

	auto has_error(const pcre_cxx::pcre_cxx_context &context) const noexcept -> bool;
	auto compilation_error() const noexcept -> const std::string;
	auto compilation_error_offset() const noexcept -> int;
	auto execution_error_code(const pcre_cxx::pcre_cxx_context &context) const noexcept -> int;
	auto extra_options() const noexcept -> pcre_extra&;
	auto captures_size(const pcre_cxx::pcre_cxx_context &context) const noexcept -> int;

	operator const pcre *() const noexcept;
	operator const pcre_extra *() const noexcept;

	auto operator [](const std::string &name) const noexcept -> int;

	auto get_compiled_data(byte_buffer_t &compiled_pattern_buffer, byte_buffer_t &compiled_study_info) const -> void;
	auto set_compiled_data(const byte_buffer_t &compiled_pattern_buffer, const byte_buffer_t &compiled_study_info) -> void;
};

}
