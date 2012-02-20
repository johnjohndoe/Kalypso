package org.kalypso.statistics.i18n;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "org.kalypso.statistics.i18n.messages"; //$NON-NLS-1$
	public static String _ERROR;
	public static String _INFORMATION;
	public static String _MODULE_SHORT_TITLE;
	public static String _MODULE_TITLE;
	public static String _WARNING;
	public static String ISysLogger_MESSAGE_FORMAT;
	public static String Error_FatalErrorTitle;
	public static String Error_GeneralErrorTitle;
	public static String Error_InternalErrorTitle;
	public static String Error_InternalErrorMessage;
	public static String Error_InternalErrorMessageWithExceptionMessage;
	public static String Error_InternalErrorRuntimeMessage;
	public static String AbstractSysLogger_0;
	public static String AbstractSysLogger_1;
	public static String DateTime_ddMMHHmm;
	public static String DateTime_ddMMyyyy;
	public static String DateTime_ddMMyyyyHHmm;
	public static String DateTime_ddMMyyyyHHmmss;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
