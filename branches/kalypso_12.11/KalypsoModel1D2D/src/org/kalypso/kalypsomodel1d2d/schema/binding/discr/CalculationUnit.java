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
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Default implementation for {@link ICalculationUnit}
 * 
 * @author Patrice Congo
 * @author Stefan Kurzbach
 */
public abstract class CalculationUnit extends Feature_Impl implements ICalculationUnit
{
  public CalculationUnit( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private FeatureList elementsInternal( )
  {
    return (FeatureList)getProperty( QN_PROPERTY_ELEMENT );
  }

  @Override
  public IFENetItem[] getElements( )
  {
    return elementsInternal().toFeatures( new IFENetItem[elementsInternal().size()] );
  }

  @Override
  public void addLinkedItem( final IFENetItem element )
  {
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$

    final FeatureList elementsInternal = elementsInternal();

    if( !elementsInternal.containsLinkTo( element ) )
      elementsInternal.addLink( element );

    element.addLinkedComplexElement( this );
  }

  @Override
  public void removeLinkedItems( final IFENetItem[] elements )
  {
    Assert.throwIAEOnNullParam( elements, "element" ); //$NON-NLS-1$

    final FeatureList elementsInternal = elementsInternal();

    elementsInternal.removeLinks( elements );

    for( final IFENetItem element : elements )
      element.removeLinkedComplexElement( this );
  }

  @Override
  public TYPE getType( )
  {
    if( this instanceof ICalculationUnit1D )
      return TYPE.TYPE1D;
    else if( this instanceof ICalculationUnit2D )
      return TYPE.TYPE2D;
    else if( this instanceof ICalculationUnit1D2D )
      return TYPE.TYPE1D2D;
    return TYPE.TYPE_NON_DEFINED;
  }

  @Override
  public List<IFELine> getContinuityLines( )
  {
    final List<IFELine> continuityLines = new ArrayList<>();
    for( final IFENetItem element : getElements() )
      if( element instanceof IFELine )
        continuityLines.add( (IFELine)element );
    return continuityLines;
  }

  @Override
  public boolean contains( final IFENetItem member )
  {
    Assert.throwIAEOnNullParam( member, "member" ); //$NON-NLS-1$

    return elementsInternal().containsLinkTo( member );
  }

  @Override
  public List<IElement1D> getElements1D( )
  {
    final List<IElement1D> list = new ArrayList<>();
    for( final IFENetItem element : getElements() )
      if( element instanceof IElement1D )
        list.add( (IElement1D)element );
    return list;
  }

  @Override
  public List<IPolyElement> getElements2D( )
  {
    final List<IPolyElement> list = new ArrayList<>();
    for( final IFENetItem element : getElements() )
    {
      if( element instanceof IPolyElement )
        list.add( (IPolyElement)element );
    }
    return list;
  }

  @Override
  public int size( )
  {
    return elementsInternal().size();
  }

  @Override
  public List<IFENetItem> query( final GM_Envelope env, List<IFENetItem> result )
  {
    if( result == null )
    {
      // FIXME: bad!
      result = new ArrayList<>( 100 );
    }

    return elementsInternal().queryResolved( env, result );
  }

  @Override
  public GM_Envelope getBoundingBox( )
  {
    return elementsInternal().getBoundingBox();
  }
}