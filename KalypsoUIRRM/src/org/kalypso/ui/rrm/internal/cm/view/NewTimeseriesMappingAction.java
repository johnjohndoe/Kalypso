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
package org.kalypso.ui.rrm.internal.cm.view;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMappingCollection;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class NewTimeseriesMappingAction extends Action
{
  private final ITimeseriesMappingCollection m_timeseriesMappings;

  private final TimeseriesMappingType m_mappingType;

  private final ITreeNodeModel m_treeModel;

  public NewTimeseriesMappingAction( final ITimeseriesMappingCollection timeseriesMappings, final TimeseriesMappingType mappingType, final ITreeNodeModel treeModel )
  {
    m_timeseriesMappings = timeseriesMappings;
    m_mappingType = mappingType;
    m_treeModel = treeModel;

    final String text = String.format( "New mapping (%s)", mappingType.getLabel() );
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

    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() == Window.OK )
    {
      try
      {
        final Feature mappingFeature = newMapping.apply( m_treeModel.getWorkspace(), m_timeseriesMappings );
        m_treeModel.refreshTree( mappingFeature );
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