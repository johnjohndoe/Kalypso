package org.kalypso.statistics.types;

import org.kalypso.statistics.command.handler.ChartDisplaySelectedTimeseriesHandler;
import org.kalypso.statistics.command.handler.CreateExtremsTimeseriesHandler;
import org.kalypso.statistics.command.handler.DeleteSelectedRecordsHandler;
import org.kalypso.statistics.command.handler.ImportNodesHandler;
import org.kalypso.statistics.command.handler.ImportTimeseriesHandler;
import org.kalypso.statistics.command.handler.NotImplementedHandler;
import org.kalypso.statistics.command.handler.PreProcessSelectedDischargeTimeseriesHandler;
import org.kalypso.statistics.command.handler.RemoveOutliersHandler;
import org.kalypso.statistics.command.handler.SelectAllHandler;
import org.kalypso.statistics.command.handler.SelectNoneHandler;
import org.kalypso.statistics.command.handler.TrendAdjustmentHandler;

public enum EToolBar {
	NEW(EStatisticsImage.TOOLBAR_ICON_NEW, NotImplementedHandler.ID), //
	SELECT_ALL(EStatisticsImage.TOOLBAR_ICON_SELECT_ALL, SelectAllHandler.ID), //
	DESELECT_ALL(EStatisticsImage.TOOLBAR_ICON_SELECT_NONE, SelectNoneHandler.ID), //
	DELETE_SELECTED(EStatisticsImage.TOOLBAR_ICON_DELETE, DeleteSelectedRecordsHandler.ID), //
	IMPORT_NODES(EStatisticsImage.TOOLBAR_ICON_IMPORT_DATA, ImportNodesHandler.ID), //
	IMPORT_TIMESERIES(EStatisticsImage.TOOLBAR_ICON_IMPORT_DATA, ImportTimeseriesHandler.ID), //
	CREATE_EXTREMS_TIMESERIES(EStatisticsImage.TOOLBAR_ICON_IMPORT_DATA, CreateExtremsTimeseriesHandler.ID), //
	DISPLAY_CHART_TIMESERIES(EStatisticsImage.TOOLBAR_ICON_CHART, ChartDisplaySelectedTimeseriesHandler.ID), //
	PREPROCESS_DISCHARGE_TIMESERIES(EStatisticsImage.TOOLBAR_ICON_PROCESSING, PreProcessSelectedDischargeTimeseriesHandler.ID), //
	TREND_ADJUSTMENT(EStatisticsImage.TOOLBAR_ICON_PROCESSING, TrendAdjustmentHandler.ID), //
	GRUBBS(EStatisticsImage.TOOLBAR_ICON_GRUBBS, RemoveOutliersHandler.ID), //

	// separator
	SEPARATOR(null, ""), //
	SEPARATOR2(null, ""), //
	SEPARATOR3(null, "");

	private final EStatisticsImage m_image;
	private final String m_commandID;

	private EToolBar(final EStatisticsImage image, final String commandID) {
		m_image = image;
		m_commandID = commandID;
	}

	public EStatisticsImage getStatisticsImage() {
		return m_image;
	}

	public String getCommandID() {
		return m_commandID;
	}
}
