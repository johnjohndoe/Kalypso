package org.kalypso.statistics.gui.perspectives;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.IPlaceholderFolderLayout;
import org.kalypso.afgui.views.WorkflowView;
import org.kalypso.chart.ui.view.ChartView;
import org.kalypso.ogc.gml.outline.ViewContentOutline;
import org.kalypso.statistics.gui.views.nodes.NodesListView;
import org.kalypso.ui.editor.featureeditor.FeatureTemplateView;
import org.kalypso.ui.editor.mapeditor.views.MapWidgetView;

/*
 * Initial page layout for "KalypsoStatistics" perspective.
 */
public class KalypsoStatisticsPerspective implements IPerspectiveFactory {

	public static final String ID = "org.kalypso.statistics.gui.perspectives.KalypsoStatisticsPerspective"; //$NON-NLS-1$

	@Override
	public void createInitialLayout(final IPageLayout layout) {
		final String editorArea = layout.getEditorArea();
		layout.setEditorAreaVisible(false);

		final IFolderLayout leftFolderTop = layout.createFolder("leftFolderTop", IPageLayout.LEFT, 0.3f, editorArea); //$NON-NLS-1$
		final IFolderLayout leftFolderBottom = layout.createFolder("leftFolderBottom", IPageLayout.BOTTOM, 0.7f, "leftFolderTop"); //$NON-NLS-1$ //$NON-NLS-2$
		final IFolderLayout centralFolderTop = layout.createFolder("centralFolderTop", IPageLayout.RIGHT, 1.0f, editorArea); //$NON-NLS-1$
		final IPlaceholderFolderLayout centralFolderBottom = layout
				.createPlaceholderFolder("centralFolderBottom", IPageLayout.BOTTOM, 0.7f, "centralFolderTop"); //$NON-NLS-1$ //$NON-NLS-2$
		final IPlaceholderFolderLayout rightFolder = layout.createPlaceholderFolder("rightFolder", IPageLayout.RIGHT, 0.7f, "centralFolderTop"); //$NON-NLS-1$ //$NON-NLS-2$

		leftFolderTop.addView(WorkflowView.ID);
		leftFolderBottom.addPlaceholder(ViewContentOutline.ID);
		centralFolderTop.addPlaceholder(NodesListView.ID);
		centralFolderTop.addPlaceholder(FeatureTemplateView.ID);
		centralFolderBottom.addPlaceholder(ChartView.ID);
		rightFolder.addPlaceholder(MapWidgetView.ID);

		layout.getViewLayout(FeatureTemplateView.ID).setCloseable(false);
		layout.getViewLayout(FeatureTemplateView.ID).setMoveable(false);
		layout.getViewLayout(MapWidgetView.ID).setCloseable(false);
		layout.getViewLayout(MapWidgetView.ID).setMoveable(false);
		layout.getViewLayout(WorkflowView.ID).setCloseable(false);
		layout.getViewLayout(WorkflowView.ID).setMoveable(false);
		layout.getViewLayout(ChartView.ID).setCloseable(true);
		layout.getViewLayout(ChartView.ID).setMoveable(true);

	}
}
