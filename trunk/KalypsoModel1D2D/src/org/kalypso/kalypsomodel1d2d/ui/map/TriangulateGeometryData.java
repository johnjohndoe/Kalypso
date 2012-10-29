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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.gml.processes.constDelaunay.ConstraintDelaunayHelper;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.Add2DElementsCommand;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GM_MultiCurve_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Gernot Belger
 */
class TriangulateGeometryData extends AbstractModelObject
{
  public static final String PROPERTY_MAX_AREA = "maxArea"; //$NON-NLS-1$

  public static final String PROPERTY_MIN_ANGLE = "minAngle"; //$NON-NLS-1$

  public static final String PROPERTY_NO_STEINER = "noSteiner"; //$NON-NLS-1$

  private Double m_maxArea = null;

  private double m_minAngle = 22;

  private boolean m_noSteiner = true;

  private GM_MultiCurve m_breaklines = null;

  private GM_Polygon m_boundaryGeom;

  private GM_TriangulatedSurface m_tin;

  private final TriangulateGeometryWidget m_widget;

  public TriangulateGeometryData( final TriangulateGeometryWidget widget )
  {
    m_widget = widget;
  }

  public Double getMaxArea( )
  {
    return m_maxArea;
  }

  public void setMaxArea( final Double maxArea )
  {
    final Double oldValue = m_maxArea;

    m_maxArea = maxArea;

    firePropertyChange( PROPERTY_MAX_AREA, oldValue, maxArea );

    finishGeometry();
  }

  public double getMinAngle( )
  {
    return m_minAngle;
  }

  public void setMinAngle( final double minAngle )
  {
    final double oldValue = m_minAngle;

    m_minAngle = minAngle;

    firePropertyChange( PROPERTY_MAX_AREA, oldValue, minAngle );

    finishGeometry();
  }

  public boolean getNoSteiner( )
  {
    return m_noSteiner;
  }

  public void setNoSteiner( final boolean noSteiner )
  {
    final boolean oldValue = m_noSteiner;

    m_noSteiner = noSteiner;

    firePropertyChange( PROPERTY_NO_STEINER, oldValue, noSteiner );

    finishGeometry();
  }

  public void addBreakLine( final GM_Curve breakline )
  {
    if( m_breaklines == null )
      m_breaklines = new GM_MultiCurve_Impl( breakline.getCoordinateSystem() );
    m_breaklines.add( breakline );

    finishGeometry();
  }

  public GM_MultiCurve getBreaklines( )
  {
    return m_breaklines;
  }

  public GM_TriangulatedSurface getTin( )
  {
    return m_tin;
  }

  public void setBoundary( final GM_Polygon boundaryGeom )
  {
    m_boundaryGeom = boundaryGeom;
  }

  void finishGeometry( )
  {
    if( m_boundaryGeom == null )
      return;

    /* prepare for exception */
    m_tin = null;

    try
    {
      // FIXME: encapsulate into a triangle.exe wrapper!
      final List<String> args = new ArrayList<>();
      if( m_maxArea != null && m_maxArea > 0 )
        args.add( "-a" + m_maxArea ); //$NON-NLS-1$

      if( m_minAngle > 0 )
        args.add( "-q" + m_minAngle ); //$NON-NLS-1$

      if( m_noSteiner )
        args.add( "-Y" ); //$NON-NLS-1$

      final GM_Triangle[] triangles = ConstraintDelaunayHelper.createGM_Triangles( m_boundaryGeom.get( 0 ).getExteriorRing(), m_breaklines == null ? null : m_breaklines.getAllCurves(), m_boundaryGeom.getCoordinateSystem(), args.toArray( new String[args.size()] ) );

      if( triangles != null && triangles.length == 0 )
      {
        // TODO: waning makes no sense!
        // m_warningRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.RefineFEGeometryWidget.5" ) ); //$NON-NLS-1$
      }
      else
        m_tin = GeometryFactory.createGM_TriangulatedSurface( triangles, triangles[0].getCoordinateSystem() );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }

    m_widget.onTinUpdated();
  }

  public void resetGeometries( )
  {
    m_breaklines = null;
    m_boundaryGeom = null;
    m_tin = null;
  }

  // FIXME: move into operation
  public void convertTriangulationToModel( )
  {
    final IKalypsoFeatureTheme theme = m_widget.getDiscTheme();
    final CommandableWorkspace workspace = theme.getWorkspace();

    try
    {
      final List<GM_Ring> elements = getTinRings();
      if( elements == null )
        return;

      final Add2DElementsCommand command = new Add2DElementsCommand( workspace, elements );
      workspace.postCommand( command );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
      SWT_AWT_Utilities.showSwtMessageBoxError( m_widget.getName(), Messages.getString( "TriangulateGeometryData_0" ) + e1.toString() ); //$NON-NLS-1$
    }
    finally
    {
      m_widget.reinit();
    }
  }

  private List<GM_Ring> getTinRings( ) throws GM_Exception
  {
    final GM_TriangulatedSurface tin = getTin();
    if( tin == null )
      return null;

    final List<GM_Ring> rings = new ArrayList<>( tin.size() );

    for( final GM_Triangle triangle : tin )
    {
      final GM_Position[] exteriorRing = triangle.getExteriorRing();

      final GM_Ring ring = GeometryFactory.createGM_Ring( exteriorRing, tin.getCoordinateSystem() );
      rings.add( ring );
    }

    return rings;
  }
}