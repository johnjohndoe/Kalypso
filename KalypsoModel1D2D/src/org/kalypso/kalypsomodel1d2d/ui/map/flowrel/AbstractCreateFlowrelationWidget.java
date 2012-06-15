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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.util.HashMap;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.FlowRelationUtilitites;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.CssParameter;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PolygonSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
@SuppressWarnings("deprecation")
public abstract class AbstractCreateFlowrelationWidget extends AbstractWidget
{
  private final int m_grabRadius = 10;

  protected IFlowRelationshipModel m_flowRelCollection = null;

  protected IKalypsoFeatureTheme m_flowTheme = null;

  protected IFEDiscretisationModel1d2d m_discModel = null;

  /* The current element (node, contiline, 1delement, ...) of the disc-model under the cursor. */
  protected IFeatureWrapper2 m_modelElement = null;

  protected IFlowRelationship m_existingFlowRelation;

  private final QName m_qnameToCreate;

  public AbstractCreateFlowrelationWidget( final String name, final String tooltip, final QName qnameToCreate )
  {
    super( name, tooltip );

    m_qnameToCreate = qnameToCreate;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  public final IFeatureWrapper2 getModelElement( )
  {
    return m_modelElement;
  }

  public final void setModelElement( final IFeatureWrapper2 modelElement )
  {
    m_modelElement = modelElement;
  }

  private void reinit( )
  {
    m_flowRelCollection = null;

    final IMapPanel mapPanel = getMapPanel();

    mapPanel.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.AbstractCreateFlowrelationWidget.0" ) ); //$NON-NLS-1$

    m_flowTheme = UtilMap.findEditableTheme( mapPanel, m_qnameToCreate );
    if( m_flowTheme == null )
      m_flowTheme = UtilMap.findEditableTheme( mapPanel, IFlowRelationship.QNAME );

    m_discModel = UtilMap.findFEModelTheme( mapPanel );
    if( m_flowTheme == null || m_discModel == null )
      return;

    final FeatureList featureList = m_flowTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    m_flowRelCollection = (IFlowRelationshipModel) parentFeature.getAdapter( IFlowRelationshipModel.class );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GM_Point currentPos = MapUtilities.transform( mapPanel, p );

    /* Grab next node */
    if( m_discModel == null )
    {
      if( m_modelElement != null )
      {
        m_modelElement = null;

        mapPanel.repaintMap();
      }
      return;
    }

    final double grabDistance = MapUtilities.calculateWorldDistance( mapPanel, currentPos, m_grabRadius );
    m_modelElement = findModelElementFromCurrentPosition( m_discModel, currentPos, grabDistance );

    /* Item has already a flow relation? */
    m_existingFlowRelation = null;
    if( m_flowRelCollection != null && m_modelElement != null )
    {
      final GM_Position flowPosition = FlowRelationUtilitites.getFlowPositionFromElement( m_modelElement );
      if( flowPosition != null )
        m_existingFlowRelation = m_flowRelCollection.findFlowrelationship( flowPosition, 0.0 );
    }

    mapPanel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    if( m_modelElement == null )
      return;

    try
    {
      final int smallRect = m_grabRadius;
      /* Node: return its position */
      if( m_modelElement instanceof IFE1D2DNode )
      {
        final GM_Point point = ((IFE1D2DNode) m_modelElement).getPoint();
        final Point nodePoint = MapUtilities.retransform( getMapPanel(), point );
        g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
        if( m_existingFlowRelation != null )
          g.fillRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
      }
      /* ContinuityLine: return middle of line */
      else if( m_modelElement instanceof IElement1D )
      {
        final IElement1D element = (IElement1D) m_modelElement;
        final GM_Curve line = (GM_Curve) element.recalculateElementGeometry();

        final LineSymbolizer symb = new LineSymbolizer_Impl();
        final Stroke stroke = new Stroke_Impl( new HashMap<String, CssParameter>(), null, null );
        stroke.setWidth( 3 );
        stroke.setStroke( new Color( 255, 0, 0 ) );
        symb.setStroke( stroke );
        final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( m_modelElement.getFeature(), line, symb );
        de.paint( g, getMapPanel().getProjection(), new NullProgressMonitor() );
      }
      else if( m_modelElement instanceof IFELine )
      {
        final IFELine element = (IFELine) m_modelElement;
        final GM_Curve line = element.getGeometry();

        final LineSymbolizer symb = new LineSymbolizer_Impl();
        final Stroke stroke = new Stroke_Impl( new HashMap<String, CssParameter>(), null, null );
        stroke.setWidth( 3 );
        stroke.setStroke( new Color( 255, 0, 0 ) );
        symb.setStroke( stroke );
        final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( m_modelElement.getFeature(), line, symb );
        de.paint( g, getMapPanel().getProjection(), new NullProgressMonitor() );
      }
      else if( m_modelElement instanceof IPolyElement )
      {
        final IPolyElement polyElement = (IPolyElement) m_modelElement;
        final GM_Surface<GM_SurfacePatch> surface = (GM_Surface<GM_SurfacePatch>) polyElement.recalculateElementGeometry();

        final PolygonSymbolizer symb = new PolygonSymbolizer_Impl();
        final Stroke stroke = new Stroke_Impl( new HashMap<String, CssParameter>(), null, null );
        stroke.setWidth( 3 );
        stroke.setStroke( new Color( 255, 0, 0 ) );
        symb.setStroke( stroke );

        final DisplayElement de = DisplayElementFactory.buildPolygonDisplayElement( m_modelElement.getFeature(), surface, symb );
        de.paint( g, getMapPanel().getProjection(), new NullProgressMonitor() );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();

    if( m_modelElement == null || m_flowRelCollection == null )
      return;

    final CommandableWorkspace workspace = m_flowTheme.getWorkspace();

    final IMapPanel mapPanel = getMapPanel();
    final GM_Position flowPositionFromElement;
    if( m_modelElement instanceof IFELine )
    {
      int countBCs = 0;
      for( final Object bcFeature : m_flowRelCollection.getWrappedList() )
      {
        final IBoundaryCondition bc = (IBoundaryCondition) ((Feature) bcFeature).getAdapter( IBoundaryCondition.class );
        if( bc == null )
          continue;
        if( bc.getParentElementID().equals( m_modelElement.getGmlID() ) )
          countBCs++;
      }
      int i = 0;
      for( final Object bcFeature : m_flowRelCollection.getWrappedList() )
      {
        final IBoundaryCondition bc = (IBoundaryCondition) ((Feature) bcFeature).getAdapter( IBoundaryCondition.class );
        if( bc == null )
          continue;
        if( bc.getParentElementID().equals( m_modelElement.getGmlID() ) )
        {
          final GM_Position position = FlowRelationUtilitites.getFlowPositionFromElement( m_modelElement, countBCs + 1, ++i );
          bc.setPosition( GeometryFactory.createGM_Point( position.getX(), position.getY(), bc.getPosition().getCoordinateSystem() ) );
        }
      }
      flowPositionFromElement = FlowRelationUtilitites.getFlowPositionFromElement( m_modelElement, countBCs + 1, countBCs + 1 );
    }
    else
      flowPositionFromElement = FlowRelationUtilitites.getFlowPositionFromElement( m_modelElement );

    /* Create flow relation at position */
    display.asyncExec( new Runnable()
    {

      @Override
      public void run( )
      {
        final Feature parentFeature = m_flowRelCollection.getFeature();
        final IRelationType parentRelation = m_flowRelCollection.getWrappedList().getParentFeatureTypeProperty();
        final IFlowRelationship flowRel = createNewFeature( workspace, parentFeature, parentRelation, m_modelElement );

        if( flowRel == null )
        {
          mapPanel.repaintMap();
          return;
        }

        final String crs = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
        flowRel.setPosition( GeometryFactory.createGM_Point( flowPositionFromElement, crs ) );

        /* Post it as an command */
        final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
        selectionManager.clear();
        final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1, flowRel.getFeature(), selectionManager, true, true );
        try
        {
          workspace.postCommand( command );
        }
        catch( final Throwable e )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( e );
          display.asyncExec( new Runnable()
          {
            @Override
            public void run( )
            {
              final Shell shell = display.getActiveShell();
              ErrorDialog.openError( shell, getName(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.AbstractCreateFlowrelationWidget.1" ), status ); //$NON-NLS-1$
            }
          } );
        }
        try
        {
          PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE ); //$NON-NLS-1$
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

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    /* Deselect all */
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();

    super.finish();
  }

  /**
   * Really create the new object.
   * 
   * @return The new object, if null, nothing happens..
   */
  protected abstract IFlowRelationship createNewFeature( final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentRelation, final IFeatureWrapper2 modelElement );

  /**
   * @param grabDistance
   *          The grab distance in world (=geo) coordinates.
   */
  protected IFeatureWrapper2 findModelElementFromCurrentPosition( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance )
  {
    final IFeatureWrapper2 lFoundElement = discModel.find1DElement( currentPos, grabDistance );
    if( lFoundElement == null )
    {
      return null;
    }
    final IFeatureWrapper2 lBuildingExisting = FlowRelationUtilitites.findBuildingElement1D( (IElement1D) lFoundElement, m_flowRelCollection );
    if( lBuildingExisting == null )
    {
      return lFoundElement;
    }
    else
    {
      return null;
    }
  }

  public IFeatureWrapper2 findModelElementFromPosition( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance )
  {
    final IFE1D2DNode node = discModel.findNode( currentPos, grabDistance );
    if( node != null )
    {
      if( FlowRelationUtilitites.findBuildingElementFromPosition( node.getPoint(), m_flowRelCollection ) != null )
        return null;
      final IFE1D2DElement[] elements = node.getElements();
      for( final IFE1D2DElement element : elements )
      {
        if( element instanceof IElement1D )
          return node;
      }
    }

    return null;
  }

}
