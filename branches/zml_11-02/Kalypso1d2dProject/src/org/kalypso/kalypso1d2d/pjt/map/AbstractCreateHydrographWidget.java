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
package org.kalypso.kalypso1d2d.pjt.map;

import java.awt.Graphics;
import java.awt.Point;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypso1d2d.pjt.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrograph;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.IHydrographCollection;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public abstract class AbstractCreateHydrographWidget extends AbstractWidget
{
  private final int m_grabRadius = 10;

  private IHydrographCollection m_hydrographCollection = null;

  private IKalypsoFeatureTheme m_hydroTheme;

  private IFEDiscretisationModel1d2d m_discModel = null;

  /* The current element (node, conti-line, 1d-element, ...) of the disc-model under the cursor. */
  private IFeatureWrapper2 m_modelElement = null;

  private IHydrograph m_existingHydrograph;

  private final QName m_qnameToCreate;

  public AbstractCreateHydrographWidget( final String name, final String tooltip, final QName qnameToCreate, final IKalypsoFeatureTheme hydroTheme )
  {
    super( name, tooltip );

    m_qnameToCreate = qnameToCreate;
    m_hydroTheme = hydroTheme;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.IMapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  private void reinit( )
  {
    m_hydrographCollection = null;

    final IMapPanel mapPanel = getMapPanel();

    mapPanel.setMessage( Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.AbstractCreateHydrographWidget.0" ) ); //$NON-NLS-1$
    if( m_hydroTheme == null )
      m_hydroTheme = UtilMap.findEditableTheme( mapPanel, m_qnameToCreate );

    m_discModel = UtilMap.findFEModelTheme( mapPanel );
    if( m_hydroTheme == null || m_discModel == null )
      return;

    final FeatureList featureList = m_hydroTheme.getFeatureList();

    final Feature parentFeature = featureList.getParentFeature();
    m_hydrographCollection = (IHydrographCollection) parentFeature.getAdapter( IHydrographCollection.class );
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

    /* Item has already a hydrograph? */
    m_existingHydrograph = null;
    if( m_hydrographCollection != null && m_modelElement != null )
    {
      final GM_Position hydroPosition = HydrographUtils.getHydroPositionFromElement( m_modelElement );
      if( hydroPosition != null )
        m_existingHydrograph = m_hydrographCollection.findHydrograph( hydroPosition, 0.0 );
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
        final GM_Point point = ((IFE1D2DNode< ? >) m_modelElement).getPoint();
        final Point nodePoint = MapUtilities.retransform( getMapPanel(), point );
        g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
        if( m_existingHydrograph != null )
          g.fillRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
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

    if( m_modelElement == null || m_hydrographCollection == null )
      return;

    final CommandableWorkspace workspace = m_hydroTheme.getWorkspace();

    final IMapPanel mapPanel = getMapPanel();
    final GM_Position hydroPositionFromElement;

    hydroPositionFromElement = HydrographUtils.getHydroPositionFromElement( m_modelElement );

    /* Create hydrograph at position */
    display.asyncExec( new Runnable()
    {
      @Override
      @SuppressWarnings("synthetic-access")
      public void run( )
      {
        final Feature parentFeature = m_hydrographCollection.getFeature();
        final IRelationType parentRelation = m_hydrographCollection.getWrappedList().getParentFeatureTypeProperty();
        final IHydrograph hydro = createNewFeature( workspace, parentFeature, parentRelation, m_modelElement );

        if( hydro == null )
        {
          mapPanel.repaintMap();
          return;
        }

        final String crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
        hydro.setLocation( GeometryFactory.createGM_Point( hydroPositionFromElement, crs ) );

        /* Post it as an command */
        final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
        final AddFeatureCommand command = new AddFeatureCommand( workspace, parentFeature, parentRelation, -1, hydro.getFeature(), selectionManager, true, true );
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
              ErrorDialog.openError( shell, getName(), Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.AbstractCreateHydrographWidget.1" ), status ); //$NON-NLS-1$
            }
          } );
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
  protected abstract IHydrograph createNewFeature( final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentRelation, final IFeatureWrapper2 modelElement );

  /**
   * @param grabDistance
   *          The grab distance in world (=geo) coordinates.
   */
  protected abstract IFeatureWrapper2 findModelElementFromCurrentPosition( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance );
}
