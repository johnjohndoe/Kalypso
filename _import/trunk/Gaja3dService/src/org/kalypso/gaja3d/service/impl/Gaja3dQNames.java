package org.kalypso.gaja3d.service.impl;

import javax.xml.namespace.QName;

public interface Gaja3dQNames {
	public static final String NS = "http://org.kalypso.gaja3d.service";

	public static final QName RP_BOUNDARY = new QName(NS, "Boundary");
	public static final QName RP_DEM_POINTS = new QName(NS, "DemPoints");
	public static final QName RP_DEM_GRID = new QName(NS, "DemGrid");
	public static final QName RP_BREAKLINES = new QName(NS, "Breaklines");
	public static final QName RP_MODEL_TIN = new QName(NS, "ModelTin");

	public static final QName RP_GRID_X = new QName(NS, "GridX");
	public static final QName RP_GRID_Y = new QName(NS, "GridY");

	public static final QName RP_EDGE_FILTER = new QName(NS, "EdgeFilter");
	public static final QName RP_SMOOTH_FILTER = new QName(NS, "SmoothFilter");
	public static final QName RP_FEATURE_DETECTOR = new QName(NS,
			"FeatureDetector");
	public static final QName RP_DISTANCE_TOLERANCE = new QName(NS,
			"DistanceTolerance");

	public static final QName RP_MIN_ANGLE = new QName(NS, "MinAngle");
	public static final QName RP_MAX_AREA = new QName(NS, "MaxArea");

	public static final QName RP_GRAM_ENDPOINT_REFERENCE = new QName("",
			"GramEndpointReference");

	public static final QName RESOURCE_PROPERTIES = new QName(NS,
			"Gaja3dResourceProperties");
	public static final QName RESOURCE_REFERENCE = new QName(NS,
			"Gaja3dResourceReference");

	public static final QName[] ALL_RPS = new QName[] { RP_BOUNDARY,
			RP_DEM_POINTS, RP_DEM_GRID, RP_BREAKLINES, RP_MODEL_TIN, RP_GRID_X,
			RP_GRID_Y, RP_EDGE_FILTER, RP_SMOOTH_FILTER, RP_FEATURE_DETECTOR,
			RP_DISTANCE_TOLERANCE, RP_MIN_ANGLE, RP_MAX_AREA,
			RP_GRAM_ENDPOINT_REFERENCE };

}