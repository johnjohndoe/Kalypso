package org.kalypso.statistics.gui.views;

import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;

public interface IStatisticsRecordLoadListener<T extends AbstractStatisticsRecordType> {

	public void loadRecord(final AbstractStatisticsRecordType element);

}
