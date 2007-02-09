package org.kalypso.ui.wizards.imports;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;


/**
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * 
 */
public class Messages
{
	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages";//$NON-NLS-1$
//	private static final String BUNDLE_NAME = "org.kalypso.ui.shapeImportWizards.utils.importRoughness.messages";

	private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

	private Messages()
	{
	}

	public static String getString(String key)
	{
		try
		{
			return RESOURCE_BUNDLE.getString(key);
		}
		catch (MissingResourceException e)
		{
			return '!' + key + '!';
		}
	}

	public static String getString(String key, Object[] params)
	{
		if (params == null) return getString(key);
		try
		{
			return MessageFormat.format(getString(key), params);
		}
		catch (Exception e)
		{
			return '!' + key + '!';
		}
	}
}
