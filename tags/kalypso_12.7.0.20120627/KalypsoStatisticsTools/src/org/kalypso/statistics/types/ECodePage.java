package org.kalypso.statistics.types;

import java.nio.charset.Charset;

public enum ECodePage {
	CP1252("CP 1252", "Windows Latin", "CP1252"), // ;
	UTF8("UTF-8", "Unicode", "UTF-8");

	private final String m_label;
	private final String m_shortLabel;
	private final Charset m_charset;
	private final String m_charsetID;

	private ECodePage(final String shortLabel, final String label, final String charsetID) {
		m_label = label;
		m_shortLabel = shortLabel;
		m_charsetID = charsetID;
		m_charset = Charset.forName(charsetID);
	}

	public String getLabel() {
		return m_label;
	}

	public String getShortLabel() {
		return m_shortLabel;
	}

	public Charset getCharset() {
		return m_charset;
	}

	public String getCharsetID() {
		return m_charsetID;
	}

}
