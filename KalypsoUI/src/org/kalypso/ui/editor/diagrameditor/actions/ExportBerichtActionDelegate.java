package org.kalypso.ui.editor.diagrameditor.actions;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.jface.action.IAction;
import org.kalypso.ogc.sensor.diagview.jfreechart.ExportableChart;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.diagrameditor.ObservationDiagramEditor;
import org.kalypso.ui.metadoc.AbstractExportActionDelegate;
import org.kalypso.ui.metadoc.ExportBerichtWizard;
import org.kalypso.ui.preferences.IKalypsoPreferences;

/**
 * @author schlienger
 */
public class ExportBerichtActionDelegate extends AbstractExportActionDelegate
{
  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( IAction action )
  {
    final ObservationDiagramEditor editor = (ObservationDiagramEditor) getEditor();
    
    final Preferences prefs = KalypsoGisPlugin.getDefault().getPluginPreferences();
    final int width = prefs.getInt( IKalypsoPreferences.CHART_EXPORT_WIDTH );
    final int height = prefs.getInt( IKalypsoPreferences.CHART_EXPORT_HEIGHT );
    
    final ExportableChart exportableChart = new ExportableChart( editor.getChart(), ExportableChart.EXT_JPEG, width, height );
    
    runExportAction( ExportBerichtWizard.class, exportableChart, editor.getSite().getShell() );
  }
}