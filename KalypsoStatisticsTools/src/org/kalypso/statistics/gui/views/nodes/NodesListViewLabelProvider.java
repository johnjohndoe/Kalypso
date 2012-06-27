package org.kalypso.statistics.gui.views.nodes;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Image;
import org.kalypso.statistics.gui.views.AbstractListViewLabelProvider;
import org.kalypso.statistics.plugin.ImagesProvider;
import org.kalypso.statistics.types.EColumnLabel;
import org.kalypso.statistics.types.ENodeProfileType;
import org.kalypso.statistics.types.EStatisticsImage;
import org.kalypso.statistics.types.data.NodeProfile;

public class NodesListViewLabelProvider extends AbstractListViewLabelProvider<NodeProfile> {
	// We use icons
	private static final Image NODE = ImagesProvider.getImage(EStatisticsImage.ICON_NODE_16);
	private static final Image STATION = ImagesProvider.getImage(EStatisticsImage.ICON_STATION_16);
	private final List<EColumnLabel> m_fields = new ArrayList<EColumnLabel>();

	public NodesListViewLabelProvider(final List<EColumnLabel> fields) {
		for (final EColumnLabel fld : fields) {
			m_fields.add(fld);
		}
	}

	@Override
	protected EColumnLabelsNodes getEnumIndex(final int columnIndex) {
		return (EColumnLabelsNodes) m_fields.get(columnIndex);
	}

	@Override
	protected String setCellText(final NodeProfile element, final EColumnLabel columnIndex) {
		final EColumnLabelsNodes index = (EColumnLabelsNodes) columnIndex;
		switch (index) {
		case SELECTION:
			return " ";
		case NAME:
			return element.getName();
		case DESCRIPTION:
			return element.getDescription();
		case NODE_TYPE:
			return element.getNodeProfileType().getLabel();
		default:
			throw new RuntimeException("Should not happen"); //$NON-NLS-1$
		}
	}

	@Override
	protected Image setCellImage(final NodeProfile element, final EColumnLabel columnIndex) {
		if (EColumnLabelsNodes.NODE_TYPE.equals(columnIndex)) {
			return ENodeProfileType.HYDROLOGICAL_NODE.equals(element.getNodeProfileType()) ? NODE : STATION;
		}
		return null;
	}

}
