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
package org.kalypso.kalypsomodel1d2d.schema.binding.discr;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.gml.binding.commons.AbstractFeatureBinder;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

public class JunctionElement extends AbstractFeatureBinder implements IJunctionElement
{
  private FeatureWrapperCollection<IFELine> m_continuityLines;

  public JunctionElement( final Feature featureToBind )
  {
    this( featureToBind, IJunctionElement.QNAME );
  }

  public JunctionElement( final Feature featureToBind, final QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
    final Object prop = featureToBind.getProperty( IJunctionElement.PROP_CONTI_LINES );
    if( prop == null )
      m_continuityLines = new FeatureWrapperCollection<IFELine>( featureToBind, IJunctionElement.QNAME, IJunctionElement.PROP_CONTI_LINES, IFELine.class );
    else
      m_continuityLines = new FeatureWrapperCollection<IFELine>( featureToBind, IFELine.class, IJunctionElement.PROP_CONTI_LINES );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IAbstractJunction#getContinuityLines()
   */
  @Override
  public List<IFELine> getContinuityLines( )
  {
    return m_continuityLines;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IJunctionContext1DToCLine#recalculateElementGeometry()
   */
  @Override
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    if( m_continuityLines.size() < 2 )
      return null;

    final List<GM_Position> elementPositions = new ArrayList<GM_Position>();

    if( m_continuityLines.size() > 2 )
      for( int i = 0; i < m_continuityLines.size(); i++ )
        elementPositions.add( m_continuityLines.get( i ).getNodes().get( 0 ).getPoint().getPosition() );
    else
      for( int i = 0; i < m_continuityLines.size(); i++ )
      {
        final GM_LineString lineString = m_continuityLines.get( i ).getGeometry().getAsLineString();
        elementPositions.add( lineString.getStartPoint().getPosition() );
        elementPositions.add( lineString.getEndPoint().getPosition() );
      }

    // close the ring
    elementPositions.add( elementPositions.get( 0 ) );

    final GM_Surface< ? extends GM_SurfacePatch> createGM_Surface = GeometryFactory.createGM_Surface( elementPositions.toArray( new GM_Position[] {} ), new GM_Position[][] {}, m_continuityLines.get( 0 ).getGeometry().getCoordinateSystem() );
    final Geometry export = JTSAdapter.export( createGM_Surface );
    final Geometry convexHull = export.convexHull();
    return JTSAdapter.wrap( convexHull );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement#addElementAsRef(org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem)
   */
  @Override
  public boolean addElementAsRef( final IFENetItem element )
  {
    return m_continuityLines.addRef( (IFELine) element );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement#getElements()
   */
  @Override
  public IFeatureWrapperCollection<IFENetItem> getElements( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement#removeElementAsRef(org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem)
   */
  @Override
  public void removeElementAsRef( final IFENetItem elment )
  {
    // TODO Auto-generated method stub
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement#isMemberOfCalculationUnit(org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit)
   */
  @Override
  public boolean isMemberOfCalculationUnit( final ICalculationUnit calculationUnit )
  {
    if( calculationUnit instanceof ICalculationUnit1D2D )
    {
      final List<IFELine> calcUnitContinuityLines = calculationUnit.getContinuityLines();
      boolean allLinesFound = true;
      boolean lineFound = false;
      for( final IFELine myLine : m_continuityLines )
      {
        if( !allLinesFound )
          break;
        lineFound = false;
        final String myLineGmlID = myLine.getGmlID();
        for( final IFELine calcUnitLine : calcUnitContinuityLines )
        {
          if( calcUnitLine.getGmlID().equals( myLineGmlID ) )
          {
            lineFound = true;
            break;
          }
        }
        allLinesFound &= lineFound;
      }
      return allLinesFound;
    }
    return false;
  }

}
