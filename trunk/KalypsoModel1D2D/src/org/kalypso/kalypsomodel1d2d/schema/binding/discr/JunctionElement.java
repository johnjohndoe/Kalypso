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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;

public class JunctionElement extends Feature_Impl implements IJunctionElement
{
  public JunctionElement( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private FeatureList getLinesInternal( )
  {
    return (FeatureList)getProperty( PROP_CONTI_LINES );
  }

  @Override
  public GM_Object recalculateElementGeometry( ) throws GM_Exception
  {
    final IFELine[] linesInternal = getElements();
    final int numLines = linesInternal.length;

    final List<GM_Position> elementPositions = new ArrayList<>();

    if( numLines > 2 )
      for( int i = 0; i < numLines; i++ )
        elementPositions.add( linesInternal[i].getNodes()[0].getPoint().getPosition() );
    else
      for( int i = 0; i < numLines; i++ )
      {
        final GM_LineString lineString = linesInternal[i].getGeometry().getAsLineString();
        elementPositions.add( lineString.getStartPoint().getPosition() );
        elementPositions.add( lineString.getEndPoint().getPosition() );
      }

    // close the ring
    elementPositions.add( elementPositions.get( 0 ) );

    final String crs = linesInternal[0].getGeometry().getCoordinateSystem();
    final GM_Polygon createGM_Surface = GeometryFactory.createGM_Surface( elementPositions.toArray( new GM_Position[] {} ), new GM_Position[][] {}, crs );
    final Geometry export = JTSAdapter.export( createGM_Surface );
    final Geometry convexHull = export.convexHull();
    return JTSAdapter.wrap( convexHull, crs );
  }

  @Override
  public void addLinkedItem( final IFELine element )
  {
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$
    final FeatureList linesInternal = getLinesInternal();
    if( !linesInternal.containsLinkTo( element ) )
      linesInternal.addLink( element );
  }

  @Override
  public IFELine[] getElements( )
  {
    final FeatureList linesInternal = getLinesInternal();
    return linesInternal.toFeatures( new IFELine[linesInternal.size()] );
  }

  @Override
  public void removeLinkedItems( final IFELine[] elements )
  {
    Assert.throwIAEOnNullParam( elements, "element" ); //$NON-NLS-1$

    final FeatureList linesInternal = getLinesInternal();

    for( final IFELine element : elements )
    {
      if( linesInternal.containsLinkTo( element ) )
        linesInternal.remove( element.getId() );
    }
  }

  @Override
  public boolean isMemberOfCalculationUnit( final ICalculationUnit calculationUnit )
  {
    if( calculationUnit instanceof ICalculationUnit1D2D )
    {
      final List<IFELine> calcUnitContinuityLines = calculationUnit.getContinuityLines();
      boolean allLinesFound = true;
      boolean lineFound = false;
      for( final IFELine myLine : getElements() )
      {
        if( !allLinesFound )
          break;
        lineFound = false;
        final String myLineGmlID = myLine.getId();
        for( final IFELine calcUnitLine : calcUnitContinuityLines )
        {
          if( calcUnitLine.getId().equals( myLineGmlID ) )
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
