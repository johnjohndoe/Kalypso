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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.awt.Graphics;
import java.awt.Point;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipCollection;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Gernot Belger
 */
public abstract class AbstractCreateFlowrelationWidget extends AbstractWidget
{
  private final int m_grabRadius = 20;

  private IFlowRelationshipCollection m_flowRelCollection = null;

  private IKalypsoFeatureTheme m_flowTheme = null;

  private IKalypsoFeatureTheme m_nodeTheme = null;

  private IFE1D2DNode m_node = null;

  private IFlowRelationship m_existingFlowRelation;

  private final QName m_qnameToCreate;

  private final Class m_binderClass;

  public AbstractCreateFlowrelationWidget( final String name, final String tooltip, final QName qnameToCreate, final Class binderClass )
  {
    super( name, tooltip );

    m_qnameToCreate = qnameToCreate;
    m_binderClass = binderClass;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reinit();
  }

  private void reinit( )
  {
    m_flowRelCollection = null;

    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();

    mapPanel.setMessage( "Klicken Sie in die Karte um einen Parameter hinzuzufügen." );

    m_flowTheme = UtilMap.findEditableThem( mapModell, m_qnameToCreate );
    if( m_flowTheme == null )
      m_flowTheme = UtilMap.findEditableThem( mapModell, IFlowRelationship.QNAME );

    m_nodeTheme = UtilMap.findEditableThem( mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    if( m_flowTheme == null || m_nodeTheme == null )
      return;

    final FeatureList featureList = m_flowTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    m_flowRelCollection = (IFlowRelationshipCollection) parentFeature.getAdapter( IFlowRelationshipCollection.class );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );

    /* Grab next node */
    if( m_nodeTheme == null )
    {
      m_node = null;
//    TODO: check if this repaint is necessary for the widget
      MapPanel panel = getMapPanel();
      if( panel != null )
        panel.repaint();
      return;
    }

    final IFEDiscretisationModel1d2d model = (IFEDiscretisationModel1d2d) m_nodeTheme.getFeatureList().getParentFeature().getAdapter( IFEDiscretisationModel1d2d.class );

    final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), currentPos, m_grabRadius * 2 );
    m_node = model.findNode( currentPos, grabDistance );

    /* Node has already a flow relation? */
    m_existingFlowRelation = null;
    if( m_flowRelCollection == null || m_node == null )
    {
      return;
    }
    m_existingFlowRelation = m_flowRelCollection.findFlowrelationship( m_node.getPoint(), 0.0 );

//  TODO: check if this repaint is necessary for the widget
    MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    if( m_node == null )
      return;

    final int smallRect = 10;
    final Point nodePoint = MapUtilities.retransform( getMapPanel(), m_node.getPoint() );
    g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
    if( m_existingFlowRelation != null )
      g.fillRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();

    final String problemMessage;
    if( m_node == null )
      problemMessage = "Kein FE-Knoten in der Nähe. Parameter können nur an Knoten hinzugefügt werden.";
    else if( m_existingFlowRelation != null )
      // TODO: maybe also select the existing parameter and open the feature view
      problemMessage = "Dieser Knoten besitzt bereits Parameter.";
    else
      problemMessage = null;

    if( problemMessage != null )
    {
      display.asyncExec( new Runnable()
      {
        public void run( )
        {
          final Shell shell = display.getActiveShell();
          MessageDialog.openWarning( shell, getName(), problemMessage );
        }
      } );
    }

    /* Check preconditions */
    if( m_node == null )
      return;
    if( m_flowRelCollection == null )
      return;

    doCreateNewobject();
    // TODO: move the stuff below in the method above

    final CommandableWorkspace workspace = m_flowTheme.getWorkspace();

    if( m_existingFlowRelation == null )
    {
      /* Create flow relation at position */
      final Feature parentFeature = m_flowRelCollection.getWrappedFeature();
      final IRelationType parentRelation = m_flowRelCollection.getWrappedList().getParentFeatureTypeProperty();
      final IFeatureType newFT = workspace.getGMLSchema().getFeatureType( m_qnameToCreate );
      final Feature newFeature = workspace.createFeature( parentFeature, parentRelation, newFT );
      final IFlowRelationship flowRel = (IFlowRelationship) newFeature.getAdapter( m_binderClass );
      flowRel.setPosition( m_node.getPoint() );

      /* Post it as an command */
      final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
      final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1, newFeature, selectionManager, true, true );
      try
      {
        workspace.postCommand( command );
      }
      catch( final Throwable e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        display.asyncExec( new Runnable()
        {
          public void run( )
          {
            final Shell shell = display.getActiveShell();
            ErrorDialog.openError( shell, getName(), "Fehler beim Hinzufügen eines Parameters", status );
          }
        } );
      }
    }
    else
    {
      /* Just select the existing element */
      final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();

      final Feature featureToSelect = m_existingFlowRelation.getWrappedFeature();
      final EasyFeatureWrapper easyToSelect = new EasyFeatureWrapper( m_flowTheme.getWorkspace(), featureToSelect, featureToSelect.getParent(), featureToSelect.getParentRelation() );

      final Feature[] featuresToRemove = FeatureSelectionHelper.getFeatures( selectionManager );
      selectionManager.changeSelection( featuresToRemove, new EasyFeatureWrapper[] { easyToSelect } );
    }

    /* Open the feature view in order to show the newly created parameters */
    display.asyncExec( new Runnable()
    {
      public void run( )
      {
        try
        {
          PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE );
        }
        catch( final Throwable pie )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( pie );
          KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
          pie.printStackTrace();
        }
      }
    } );
  }

  protected abstract void doCreateNewobject( );
}
