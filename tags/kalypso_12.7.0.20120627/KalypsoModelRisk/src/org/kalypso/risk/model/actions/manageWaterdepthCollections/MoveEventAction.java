/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Event;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.schema.binding.IAnnualCoverageCollection;
import org.kalypso.risk.model.schema.binding.IRasterDataModel;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.ui.editor.gmleditor.command.MoveFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 */
public class MoveEventAction extends Action implements IUpdateable
{
  private final IScenarioDataProvider m_dataProvider;

  private final TreeViewer m_eventViewer;

  private final int m_amount;

  public MoveEventAction( final IScenarioDataProvider dataProvider, final TreeViewer eventViewer, final int amount )
  {
    m_dataProvider = dataProvider;
    m_eventViewer = eventViewer;
    m_amount = amount;
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_eventViewer.getSelection();
    if( selection.size() != 1 )
      return;

    final Feature selectedFeature = (Feature) selection.getFirstElement();

    final Feature parentFeature = selectedFeature.getOwner();
    final IPropertyType pt = selectedFeature.getParentRelation();

    final List< ? > featureList = (List< ? >) parentFeature.getProperty( pt );
    final int newIndex = featureList.indexOf( selectedFeature ) + m_amount;
    if( newIndex < 0 || newIndex >= featureList.size() )
      return;

    final MoveFeatureCommand command = new MoveFeatureCommand( parentFeature, pt, selectedFeature, m_amount );

    final IScenarioDataProvider sdProvider = m_dataProvider;
    try
    {
      sdProvider.postCommand( IRasterDataModel.class.getName(), command );
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoRiskPlugin.getDefault().getLog().log( status );
      ErrorDialog.openError( event.display.getActiveShell(), Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.54" ), Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.55" ), status ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /* fire selection event, so all actions get updated */
    m_eventViewer.setSelection( new StructuredSelection( selectedFeature ) );
  }

  @Override
  public void update( )
  {
    final IStructuredSelection selection = (IStructuredSelection) m_eventViewer.getSelection();

    final boolean enabled = calculateEnabled( selection );
    setEnabled( enabled );
  }

  private boolean calculateEnabled( final IStructuredSelection selection )
  {
    if( selection.size() != 1 )
      return false;

    final Object selectedElement = selection.getFirstElement();
    final boolean isEvent = selectedElement instanceof IAnnualCoverageCollection;
    if( !isEvent )
      return false;

    final ITreeContentProvider contentProvider = (ITreeContentProvider) m_eventViewer.getContentProvider();
    final Object[] allElements = contentProvider.getElements( m_eventViewer.getInput() );
    final int index = ArrayUtils.indexOf( allElements, selectedElement );
    if( index == -1 )
      return false;

    final int movedIndex = index + m_amount;
    return movedIndex >= 0 && movedIndex < allElements.length;
  }
}