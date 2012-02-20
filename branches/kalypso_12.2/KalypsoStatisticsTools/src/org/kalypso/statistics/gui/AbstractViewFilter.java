package org.kalypso.statistics.gui;

import java.util.regex.PatternSyntaxException;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

public abstract class AbstractViewFilter extends ViewerFilter {

	// Search must be a substring of the existing value
	//
	// (?i) - By default, case-insensitive matching assumes that only
	// characters in the US-ASCII charset are being matched. Unicode-aware
	// case-insensitive matching can be enabled by specifying the
	// UNICODE_CASE flag in conjunction with this flag.
	//
	// (?u) - When this flag is specified then case-insensitive matching,
	// when enabled by the CASE_INSENSITIVE flag, is done in a manner
	// consistent with the Unicode Standard. By default, case-insensitive
	// matching assumes that only characters in the US-ASCII charset are
	// being matched
	private final String SEARCH_PATTERN = ".*(?i)(?u)%s.*";

	private String m_searchPattern = String.format(SEARCH_PATTERN, "");
	private String m_searchText = "";

	public final void setSearchText(final String s) {
		m_searchText = s != null ? s : "";
		m_searchPattern = String.format(SEARCH_PATTERN, m_searchText);
	}

	/**
	 * Returns search text
	 * 
	 * @param raw
	 *            if <code>true</code>, raw text is returned (as typed by user),
	 *            otherwise text with pattern is returned
	 */
	public final String getSearchText(final boolean raw) {
		return raw ? m_searchText : m_searchPattern;
	}

	@Override
	public final boolean select(final Viewer viewer, final Object parentElement, final Object element) {
		if (m_searchText == null || m_searchText.length() == 0) {
			return true;
		}
		try {
			return match(element, m_searchPattern);
		} catch (final PatternSyntaxException e) {
			return false;
		}
	}

	protected abstract boolean match(final Object element, final String searchPattern);
}
