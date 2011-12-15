package org.kalypso.statistics.utils;

import java.text.SimpleDateFormat;

import org.kalypso.statistics.logger.ConsoleLogger;
import org.kalypso.statistics.logger.ISysLogger;

public final class AppUtils {

	public static String APPLICATION_TITLE = "Kalypso Statistics";

	public static final SimpleDateFormat DISPLAY_DATETIME_FORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
	public static final SimpleDateFormat DISPLAY_DATE_FORMAT = new SimpleDateFormat("dd.MM.yyyy");
	public static final SimpleDateFormat DISPLAY_TIME_FORMAT_HH_MM_SS = new SimpleDateFormat("HH:mm:ss");
	public static final SimpleDateFormat DISPLAY_TIME_FORMAT_HH_MM = new SimpleDateFormat("HH:mm");

	private static ISysLogger LOGGER = null;

	private AppUtils() {
	}

	public static void setLogger(final ISysLogger logger) {
		LOGGER = logger;
	}

	public static ISysLogger getLogger() {
		if (LOGGER == null) {
			return new ConsoleLogger();
		}
		return LOGGER;
	}

}
