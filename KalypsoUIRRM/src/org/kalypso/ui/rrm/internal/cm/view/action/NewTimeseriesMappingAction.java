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
package org.kalypso.ui.rrm.internal.cm.view.action;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.cm.view.EditTimeseriesMappingWizard;
import org.kalypso.ui.rrm.internal.cm.view.TimeseriesMappingBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class NewTimeseriesMappingAction extends Action
{
  private final TimeseriesMappingType m_mappingType;

  private final ITreeNodeModel m_treeModel;

  public NewTimeseriesMappingAction( final TimeseriesMappingType mappingType, final ITreeNodeModel treeModel )
  {
    m_mappingType = mappingType;
    m_treeModel = treeModel;

    final String text = String.format( Messages.getString("NewTimeseriesMappingAction_0"), mappingType.getLabel() ); //$NON-NLS-1$
    setText( text );

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.GENERATOR_NEW_LINEAR_SUM ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final TimeseriesMappingBean newMapping = new TimeseriesMappingBean( m_mappingType );
    newMapping.initFromNaModel();

    final EditTimeseriesMappingWizard wizard = new EditTimeseriesMappingWizard( newMapping );
    wizard.setWindowTitle( getText() );
    wizard.setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoUIRRMPlugin.getDefault(), EditTimeseriesMappingWizard.class.getName() ) );

    final WizardDialog2 dialog = new WizardDialog2( shell, wizard );
    dialog.setRememberSize( true );
    if( dialog.open() == Window.OK )
    {
      try
      {
        final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
        final CommandableWorkspace mappingsWorkspace = dataProvider.getCommandableWorkSpace( IUiRrmWorkflowConstants.SCENARIO_DATA_TIMESERIES_MAPPINGS );
        final ITimeseriesMappingCollection timeseriesMappings = (ITimeseriesMappingCollection) mappingsWorkspace.getRootFeature();

        final Feature feature = newMapping.apply( mappingsWorkspace, timeseriesMappings );
        m_treeModel.refreshTree( feature );
      }
      catch( final Exception e )
      {
        // should never happen
        e.printStackTrace();
        final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "Failed ot create new mapping", e ); //$NON-NLS-1$
        StatusDialog.open( shell, status, wizard.getWindowTitle() );
      }
    }
  }
}