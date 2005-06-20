/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
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
    final ObservationDiagramEditor editor = (ObservationDiagramEditor)getEditor();

    final Preferences prefs = KalypsoGisPlugin.getDefault().getPluginPreferences();
    final int width = prefs.getInt( IKalypsoPreferences.CHART_EXPORT_WIDTH );
    final int height = prefs.getInt( IKalypsoPreferences.CHART_EXPORT_HEIGHT );

    final ExportableChart exportableChart = new ExportableChart( editor.getChart(), ExportableChart.EXT_JPEG, width,
        height );

    runExportAction( ExportBerichtWizard.class, exportableChart, editor.getSite().getShell() );
  }
}