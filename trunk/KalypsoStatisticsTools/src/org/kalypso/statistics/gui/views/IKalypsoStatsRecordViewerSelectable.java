package org.kalypso.statistics.gui.views;

import java.util.List;
import java.util.Map;

import org.eclipse.swt.widgets.ToolItem;
import org.kalypso.statistics.types.CommandToolbarItem;
import org.kalypso.statistics.types.EToolBar;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;

public interface IKalypsoStatsRecordViewerSelectable<T extends AbstractStatisticsRecordType> {

	public static enum TYPE {
		DELETE
	}

	public void selectAll();

	public void deselectAll();

	public List<T> getSelection();

	/**
	 * Returns custom viewer messages for message dialog, for different action
	 * types.
	 * 
	 * @param type
	 * @return String[0] - Dialog title<br>
	 *         String[0] - Dialog message
	 */
	public String[] getCustomMessage(final TYPE type);

	/**
	 * Answers if selected records can be deleted.
	 * 
	 * @return Error message if records are not deletable, or <code>null</code>
	 *         if records can be deleted.
	 */
	public String cannotDeleteRecordsErrMessage(final List<? extends AbstractStatisticsRecordType> records);

	public Class<T> getRecordType();

	public Map<EToolBar, CommandToolbarItem> getToolbarEnabledItemsMap();

	public Map<EToolBar, ToolItem> getToolbarItemsMap();

}
