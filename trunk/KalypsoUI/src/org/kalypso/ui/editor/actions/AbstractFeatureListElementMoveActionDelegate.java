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
package org.kalypso.ui.editor.actions;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter2;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.gmleditor.util.command.MoveFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author Gernot Belger
 */
public class AbstractFeatureListElementMoveActionDelegate implements IObjectActionDelegate, ModellEventListener
{
  private final int m_step;

  private IWorkbenchPart m_targetPart;

  private Feature m_selectedFeature = null;

  private CommandableWorkspace m_workspace = null;

  private ISelection m_selection;

  private IAction m_action;

  private final PartAdapter2 m_partAdapter = new PartAdapter2()
  {
    @Override
    public void partClosed( final IWorkbenchPartReference partRef )
    {
      AbstractFeatureListElementMoveActionDelegate.this.partClosed( partRef.getPart( false ) );
    }
  };

  protected AbstractFeatureListElementMoveActionDelegate( final int step )
  {
    m_step = step;
  }

  protected void partClosed( final IWorkbenchPart part )
  {
    if( m_targetPart == part )
    {
      m_targetPart.getSite().getPage().removePartListener( m_partAdapter );

      if( m_workspace != null )
        m_workspace.removeModellListener( AbstractFeatureListElementMoveActionDelegate.this );
    }
  }

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    if( m_targetPart != null )
      m_targetPart.getSite().getPage().removePartListener( m_partAdapter );

    m_action = action;
    m_targetPart = targetPart;

    if( m_targetPart != null )
      m_targetPart.getSite().getPage().addPartListener( m_partAdapter );

    if( m_targetPart == null && m_workspace != null )
      m_workspace.removeModellListener( this );
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    m_action = action;

    if( m_selectedFeature == null || m_workspace == null )
      return;

    final IRelationType rt = m_selectedFeature.getParentRelation();
    if( rt != null )
    {
      final List list = (List) m_selectedFeature.getParent().getProperty( rt );
      if( list != null )
      {
        final MoveFeatureCommand command = new MoveFeatureCommand( m_selectedFeature.getParent(), rt, m_selectedFeature, m_step );
        try
        {
          m_workspace.postCommand( command );
        }
        catch( final Exception e )
        {
          final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "", e );
          KalypsoGisPlugin.getDefault().getLog().log( status );

          // we are in the ui-thread so we get a shell here
          final Shell shell = m_targetPart.getSite().getShell();
          if( shell != null )
            ErrorDialog.openError( shell, action.getText(), "Fehler beim Verschieben des Objektes", status );
        }
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_action = action;
    m_selection = selection;

    action.setEnabled( false );

    if( m_workspace != null )
      m_workspace.removeModellListener( this );

    m_workspace = null;
    m_selectedFeature = null;

    /* The action will be enabled iff we have a moveable feature. */
    if( !selection.isEmpty() && selection instanceof IFeatureSelection )
    {
      final IFeatureSelection featureSelection = (IFeatureSelection) selection;
      m_selectedFeature = FeatureSelectionHelper.getFirstFeature( featureSelection );
      m_workspace = featureSelection.getWorkspace( m_selectedFeature );
      if( m_workspace != null )
        m_workspace.addModellListener( this );
      // it is always a Feature (objectcontribution)

      final IRelationType rt = m_selectedFeature.getParentRelation();
      if( rt != null )
      {
        final List list = (List) m_selectedFeature.getParent().getProperty( rt );
        if( list != null )
        {
          final int currentIndex = list.indexOf( m_selectedFeature );
          if( currentIndex != -1 )
          {
            final int newIndex = currentIndex + m_step;
            if( newIndex >= 0 && newIndex < list.size() )
              action.setEnabled( true );
          }
        }
      }
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent instanceof FeatureStructureChangeModellEvent )
    {
      final FeatureStructureChangeModellEvent fscm = (FeatureStructureChangeModellEvent) modellEvent;
      final Feature[] parentFeatures = fscm.getParentFeatures();
      for( final Feature feature : parentFeatures )
      {
        if( feature == m_selectedFeature.getParent() )
          selectionChanged( m_action, m_selection );
      }
    }
  }

}
