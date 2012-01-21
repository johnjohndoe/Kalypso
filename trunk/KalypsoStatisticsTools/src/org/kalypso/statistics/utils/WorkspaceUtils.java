package org.kalypso.statistics.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.service.datalocation.Location;
import org.kalypso.statistics.db.handler.DBHandlerSysVars;
import org.kalypso.statistics.types.KalypsoStatisticsException;

public final class WorkspaceUtils {

	public static enum SYSVARS {
		SYSVOL, //
		SYSPATH, //
		SYSTIME, //
		LICENCE, //
		SYSSQLUPDATETIME
	}

	public static enum OS {
		WINDOWS, //
		LINUX, //
		SOLARIS, //
		MAC, //
		UNKNOWN
	}

	private WorkspaceUtils() {
		// DO NOT INSTANTIATE!
	}

	public static final InputStream getResourceStream(final String path) {
		return WorkspaceUtils.class.getResourceAsStream(path);
	}

	public static final File getSystemTempFolder() {
		try {
			final String string = System.getProperty("java.io.tmpdir");
			final File folder = new File(string);
			return folder;
		} catch (final NullPointerException e) {
			AppUtils.getLogger().logException(e);
			return null;
		}
	}

	public static final File getWorkspaceRootFolder() throws KalypsoStatisticsException {
		final Location instanceLocation = Platform.getInstanceLocation();
		if (instanceLocation == null)
			throw new KalypsoStatisticsException("Undefined workspace location.");
		try {
			final URI uri = new URI(instanceLocation.getURL().toExternalForm().replace(" ", "%20"));
			return new File(uri);
		} catch (final URISyntaxException e) {
			throw new KalypsoStatisticsException("Workspace URI mailformed exception.");
		}
	}

	public static void initLocation() throws IOException {
		final Map<String, String> map = new HashMap<String, String>();
		final File file = new File(".");
		final String path = file.getAbsolutePath().toUpperCase();
		map.put(SYSVARS.SYSPATH.name(), path);
		if (OS.WINDOWS.equals(getOS())) {
			final String letter = path.substring(0, path.indexOf(':'));
			final Process p = Runtime.getRuntime().exec(String.format("cmd /C vol %s:", letter));
			final BufferedReader input = new BufferedReader(new InputStreamReader(p.getInputStream()));
			String result = "";
			String line;
			while ((line = input.readLine()) != null) {
				result += line;
			}
			input.close();
			final String vol = result.substring(result.lastIndexOf(' ') + 1);
			map.put(SYSVARS.SYSVOL.name(), vol);
		} else {
			map.put(SYSVARS.SYSVOL.name(), "NONE");
		}
		map.put(SYSVARS.SYSTIME.name(), Long.toString(System.currentTimeMillis()));
		DBHandlerSysVars.saveVars(map);
	}

	public static final OS getOS() {

		// druga mogucnost: SWT.getPlatform();
		// Nije isto:
		// za Win7, dobija se:
		// SWT.getPlatform(): win32
		// System.getProperty("os.name"): Windows 7
		final String os = System.getProperty("os.name").toLowerCase();
		if (os.contains("windows"))
			return OS.WINDOWS;
		if (os.contains("linux"))
			return OS.LINUX;
		if (os.contains("mac"))
			return OS.MAC;
		if (os.contains("solaris"))
			return OS.SOLARIS;
		return OS.UNKNOWN;
	}

//	public static void backUpDatabase(final Connection conn) throws SQLException {
//		final SimpleDateFormat todaysDate = new SimpleDateFormat("yyyy-MM-dd");
//		final String backupdirectory = getBackupsFolder().getAbsolutePath() + File.separator + todaysDate.format((Calendar.getInstance()).getTime());
//
//		final CallableStatement cs = conn.prepareCall("CALL SYSCS_UTIL.SYSCS_BACKUP_DATABASE(?)");
//		cs.setString(1, backupdirectory);
//		cs.execute();
//		cs.close();
//	}

}
