package org.kalypso.statistics.gui.views;

import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;


public interface IStatisticsRecordListView<T extends AbstractStatisticsRecordType> {

	public String getID();

	public boolean isApplicableType(final Object object);

	public void setInputSource();

	public void refresh();

	public void addCustomSelectionListener(final IStatisticsRecordLoadListener<T> listener);

	public void removeCustomSelectionListener(final IStatisticsRecordLoadListener<T> listener);

}
