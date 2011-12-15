package org.kalypso.statistics.utils;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import org.kalypso.statistics.types.ECodePage;

public class StringUtils {

	public static final String join(final String[] strings, final String separator, final boolean excludeEmpty, final boolean trim) {
		if (strings == null || strings.length == 0)
			return "";
		final String sep = separator == null ? "" : separator;
		final StringBuffer buffer = new StringBuffer();
		for (final String string : strings) {
			final String entry = trim ? string.trim() : string;
			if (excludeEmpty && entry.length() == 0)
				continue;
			buffer.append(entry);
			buffer.append(sep);
		}
		if (buffer.length() > 0)
			buffer.setLength(buffer.length() - sep.length());
		return buffer.toString();
	}

	public static final String[] split(final String string, final String separator, final boolean excludeEmpty, final boolean trim) {
		if (string == null || string.length() == 0)
			return new String[0];
		final String[] strings = string.split(separator);
		if (strings == null || strings.length == 0)
			return new String[0];
		final List<String> list = new ArrayList<String>();
		for (final String str : strings) {
			final String entry = trim ? str.trim() : str;
			if (excludeEmpty && entry.length() == 0)
				continue;
			list.add(entry);
		}
		return list.toArray(new String[list.size()]);
	}

	public static final String unicodeToUTF8(final String unicodeString) {
		final Charset charset = ECodePage.UTF8.getCharset();
		final byte[] utf8 = unicodeString.getBytes(charset);
		return new String(utf8, charset);
	}

	/**
	 * Searches "searchTerm" in "content" and returns an array of int pairs
	 * (index, length) for each occurrence. The search is case-sensitive. The
	 * consecutive occurrences are merged together.<code>
	Examples:
	 content = "123123x123"
	 searchTerm = "1"
	 --> [0, 1, 3, 1, 7, 1]
	 content = "123123x123"
	 searchTerm = "123"
	 --> [0, 6, 7, 3]
	</code>
	 * 
	 * @param searchTerm
	 *            can be null or empty. int[0] is returned in this case!
	 * @param content
	 *            a not-null string (can be empty!)
	 * @param ignoreCase
	 *            if set, comparation is performed ignoring case
	 * @return an array of int pairs (index, length)
	 */
	public static int[] getSearchTermOccurrences(final String searchTerm, final String content, final boolean ignoreCase) {
		if (searchTerm == null || searchTerm.length() == 0 || content == null) {
			return new int[0];
		}
		final List<Integer> list = new ArrayList<Integer>();
		final int searchTermLength = searchTerm.length();
		int index;
		int fromIndex = 0;
		int lastIndex = -1;
		int lastLength = 0;
		final String mContent = ignoreCase ? content.toLowerCase() : content;
		final String mSearchTerm = ignoreCase ? searchTerm.toLowerCase() : searchTerm;
		while (true) {
			index = mContent.indexOf(mSearchTerm, fromIndex);
			if (index == -1) {
				// no occurrence of "searchTerm" in "content" starting from
				// index "fromIndex"
				if (lastIndex != -1) {
					// but there was a previous occurrence
					list.add(Integer.valueOf(lastIndex));
					list.add(Integer.valueOf(lastLength));
				}
				break;
			}
			if (lastIndex == -1) {
				// the first occurrence of "searchTerm" in "content"
				lastIndex = index;
				lastLength = searchTermLength;
			} else {
				if (lastIndex + lastLength == index) {
					// the current occurrence is right after the previous
					// occurrence
					lastLength += searchTermLength;
				} else {
					// there is at least one character between the current
					// occurrence and the previous one
					list.add(Integer.valueOf(lastIndex));
					list.add(Integer.valueOf(lastLength));
					lastIndex = index;
					lastLength = searchTermLength;
				}
			}
			fromIndex = index + searchTermLength;
		}
		final int n = list.size();
		final int[] result = new int[n];
		for (int i = 0; i != n; i++) {
			result[i] = list.get(i);
		}
		return result;
	}
}
