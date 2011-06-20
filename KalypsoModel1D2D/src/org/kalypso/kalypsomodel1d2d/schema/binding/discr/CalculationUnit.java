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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.Feature_Impl;

/**
 * Default implementation for {@link ICalculationUnit}
 * 
 * @author Patrice Congo
 * @author Stefan Kurzbach
 * 
 */
public class CalculationUnit extends Feature_Impl implements ICalculationUnit
{
  public final static QName QN_PROPERTY_ELEMENT = new QName( UrlCatalog1D2D.MODEL_1D2D_NS, "element" ); //$NON-NLS-1$

  private final FeatureBindingCollection<IFENetItem> m_elements = new FeatureBindingCollection<IFENetItem>( this, IFENetItem.class, QN_PROPERTY_ELEMENT );

  public CalculationUnit( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IFEComplexElement#getElements()
   */
  @Override
  public FeatureBindingCollection<IFENetItem> getElements( )
  {
    return m_elements;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement#addElementAsRef(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement)
   */
  @Override
  public boolean addElementAsRef( final IFENetItem element )
  {
    Assert.throwIAEOnNullParam( element, "element" ); //$NON-NLS-1$
    return m_elements.addRef( element );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DComplexElement#removeElementAsRef(org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DElement)
   */
  @Override
  public void removeElementAsRef( final IFENetItem element )
  {
    m_elements.remove( element );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit#getType()
   */
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
    final List<IFELine> continuityLines = new ArrayList<IFELine>();
    for( final IFENetItem element : m_elements )
      if( element instanceof IFELine )
        continuityLines.add( (IFELine) element );
    return continuityLines;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit#contains(org.kalypsodeegree.model.feature.binding.Feature)
   */
  @Override
  public boolean contains( final IFENetItem member )
  {
    return m_elements.contains( member );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit#getElements1D()
   */
  @Override
  public List<IElement1D> getElements1D( )
  {
    final List<IElement1D> list = new ArrayList<IElement1D>();
    for( final IFENetItem element : m_elements )
      if( element instanceof IElement1D )
        list.add( (IElement1D) element );
    return list;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit#getElements2D()
   */
  @Override
  public List<IPolyElement> getElements2D( )
  {
    final List<IPolyElement> list = new ArrayList<IPolyElement>();
    for( final IFENetItem element : m_elements )
    {
      if( element instanceof IPolyElement )
        list.add( (IPolyElement) element );
    }
    return list;
  }
}
