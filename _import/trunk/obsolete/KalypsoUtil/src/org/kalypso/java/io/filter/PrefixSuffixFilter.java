package org.kalypso.java.io.filter;

import java.io.File;
import java.io.FilenameFilter;

/**
 * A class that filters file names according to a prexif and a suffix.
 * A file which name begins with prefix and ends with suffix will be accepted.
 * 
 * @author schlienger
 */
public class PrefixSuffixFilter implements FilenameFilter
{
	private final String m_prefix;
	private final String m_suffix;
	
	/**
	 * constructor
	 */
	public PrefixSuffixFilter(String prefix, String suffix)
	{
		if( prefix == null || suffix == null )
			throw new IllegalArgumentException("Parameters prefix/suffix must not be null");
			
		m_prefix = prefix;
		m_suffix = suffix;
	}

	/**
	 * @see java.io.FilenameFilter#accept(java.io.File, java.lang.String)
	 */
	public boolean accept(File dir, String name)
	{
		if( name.startsWith(m_prefix) && name.endsWith(m_suffix) )
			return true;
		else
			return false;
	}
}
