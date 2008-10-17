package org.kalypso.gaja3d.service.impl;

import javax.xml.namespace.QName;

public interface WPSQNames {
	public static final String NS_OWS = "http://www.opengeospatial.net/ows";
	public static final String NS_WPS = "http://www.opengeospatial.net/wps";

	public static final QName GET_CAPABILITIES = new QName(NS_OWS,
			"GetCapabilities");
	public static final QName CAPABILITIES = new QName(NS_WPS, "Capabilities");

	public static final QName EXECUTE = new QName(NS_WPS, "Execute");
	public static final QName EXECUTE_RESPONSE = new QName(NS_WPS,
			"ExecuteResponse");
	public static final QName DESCRIBE_PROCESS = new QName(NS_WPS,
			"DescribeProcess");;
}
