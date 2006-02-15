package org.kalypso.dss.planerclient.browser;


public interface IKalypsoLinkHandler {

	public final static String PERSPECTIV_PROTOCOL = "perspective:";

	public final static String LAUNCH_PROTOCOL = "launch:";

	public static final String SEPERATOR = "&";

	public boolean handleLaunchProtocol(Object link);

	public boolean handlePerspectiveProtocol(String path);

}
