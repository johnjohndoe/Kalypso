/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.model.ModelDtmWizard;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
class PreviewModelDtm extends Action
{
  private final ApplyElevationWidgetDataModel m_dataModel;

  PreviewModelDtm( final ApplyElevationWidgetDataModel dataModel )
  {
    m_dataModel = dataModel;

    setText( Messages.getString("PreviewModelDtm.0") ); //$NON-NLS-1$
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final IMapPanel mapPanel = m_dataModel.getMapPanel();

    final AbstractMapPart mapView = (AbstractMapPart) UtilMap.getMapView();
    final IFEDiscretisationModel1d2d discModel = UtilMap.findFEModelTheme( mapPanel );

    final IFile dtmFile = findFile( "models/model_tin.gz" ); //$NON-NLS-1$
    if( dtmFile == null )
      return;

    final IFile styleFile = findFile( "styles/model_tin.sld" ); //$NON-NLS-1$
    if( styleFile == null )
      return;

    final ModelDtmWizard wizard = new ModelDtmWizard( dtmFile, styleFile, discModel, mapView );
    final WizardDialog dialog = new WizardDialog( shell, wizard );
    dialog.open();
  }

  private IFile findFile( final String path )
  {
    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    final IContainer scenarioFolder = dataProvider.getScenarioFolder();

    return scenarioFolder.getFile( Path.fromOSString( path ) );
  }
}