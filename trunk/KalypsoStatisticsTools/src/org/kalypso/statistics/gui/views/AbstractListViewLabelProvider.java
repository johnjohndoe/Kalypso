package org.kalypso.statistics.gui.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.StyledCellLabelProvider;
import org.eclipse.jface.viewers.ViewerCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.kalypso.statistics.types.EColumnLabel;
import org.kalypso.statistics.types.data.AbstractStatisticsRecordType;
import org.kalypso.statistics.utils.StringUtils;

public abstract class AbstractListViewLabelProvider<T extends AbstractStatisticsRecordType> extends StyledCellLabelProvider {

	private static Color COLOR_YELLOW = Display.getCurrent().getSystemColor(SWT.COLOR_YELLOW);

	@Override
	public void update(final ViewerCell cell) {
		if (PlatformUI.getWorkbench().isClosing())
			return;
		final T element = (T) cell.getElement();
		final int index = cell.getColumnIndex();
		final EColumnLabel columnIndex = getEnumIndex(index);
		final String cellText = setCellText(element, columnIndex);
		if (cellText == null || cellText.length() == 0)
			cell.setText("-");
		else
			cell.setText(cellText);

		// apply filtering style
		final IWorkbenchPart activePart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getActivePart();
		if (activePart instanceof AbstractRecordListView<?>) {
			final String search = ((AbstractRecordListView<?>) activePart).getFilter().getSearchText(true);
			if (search.length() > 0) {
				final int intRangesCorrectSize[] = StringUtils.getSearchTermOccurrences(search, cellText, true);
				final List<StyleRange> styleRange = new ArrayList<StyleRange>();
				for (int i = 0; i < intRangesCorrectSize.length / 2; i++) {
					final int start = intRangesCorrectSize[i];
					final int length = intRangesCorrectSize[++i];
					final StyleRange myStyledRange = new StyleRange(start, length, null, COLOR_YELLOW);
					styleRange.add(myStyledRange);
				}
				cell.setStyleRanges(styleRange.toArray(new StyleRange[styleRange.size()]));
			} else {
				cell.setStyleRanges(null);
			}
		}

		final Image cellImage = setCellImage(element, columnIndex);
		if (cellImage != null)
			cell.setImage(cellImage);
		final Color bgColor = setBackgroundColor(element, columnIndex);
		if (bgColor != null)
			cell.setBackground(bgColor);
		final Color fgColor = setForegroundColor(element, columnIndex);
		if (fgColor != null)
			cell.setForeground(fgColor);
		super.update(cell);
	}

	protected Color setForegroundColor(final T element, final EColumnLabel columnIndex) {
		return null;
	}

	protected Color setBackgroundColor(final T element, final EColumnLabel columnIndex) {
		return null;
	}

	/**
	 * Successors should implement this method as
	 * <code>E.values()[columnIndex];</code>
	 * 
	 * @param columnIndex
	 * @return
	 */
	protected abstract EColumnLabel getEnumIndex(final int columnIndex);

	protected abstract String setCellText(final T element, final EColumnLabel columnIndex);

	/**
	 * Returns cell image, or null if cell have no image
	 * 
	 * @param element
	 * @param columnIndex
	 * @return
	 */
	protected abstract Image setCellImage(final T element, final EColumnLabel columnIndex);

}
