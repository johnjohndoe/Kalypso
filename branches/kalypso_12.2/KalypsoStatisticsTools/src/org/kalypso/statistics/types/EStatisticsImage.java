package org.kalypso.statistics.types;

public enum EStatisticsImage {
	TOOLBAR_ICON_DELETE("resources/icons/x20/delete.png"), //
	TOOLBAR_ICON_NEW("resources/icons/x20/new.png"), //
	TOOLBAR_ICON_SAVE("resources/icons/x20/save.jpg"), //
	TOOLBAR_ICON_IMPORT_DATA("resources/icons/x20/import.png"), //
	TOOLBAR_ICON_IMPORT_EDIT("resources/icons/x20/edit.gif"), //
	TOOLBAR_ICON_SELECT_ALL("resources/icons/x20/select_all.gif"), //
	TOOLBAR_ICON_SELECT_NONE("resources/icons/x20/select_none.gif"), //
	TOOLBAR_ICON_CHART("resources/icons/x20/chart.gif"), //
	TOOLBAR_ICON_PROCESSING("resources/icons/x20/processing.gif"), //
	TOOLBAR_ICON_GRUBBS("resources/icons/x20/arrowsRight.png"), //

	ICON_REMOVEFILTER_16("resources/icons/x16/noFilter.png"), //

	ICON_NODE_16("resources/icons/x16/node.png"), //
	ICON_STATION_16("resources/icons/x16/measurement.png"), //
	ICON_EXTREMS_16("resources/icons/x16/stats1.png"), //
	ICON_UNKNOWN_16("resources/icons/x16/unknown.png"), //

	ICON_RAINFALL_16("resources/icons/x16/rainfall.png"), //
	ICON_RUNOFF_16("resources/icons/x16/runoff.png"), //
	ICON_WATERLEVEL_16("resources/icons/x16/level.png"), //
	ICON_THUNDERSTORM_16("resources/icons/x16/thunderstorm.png"); //

	private final String m_imgPath;

	private EStatisticsImage(final String imgPath) {
		m_imgPath = imgPath;
	}

	public String getImagePath() {
		return m_imgPath;
	}
}
