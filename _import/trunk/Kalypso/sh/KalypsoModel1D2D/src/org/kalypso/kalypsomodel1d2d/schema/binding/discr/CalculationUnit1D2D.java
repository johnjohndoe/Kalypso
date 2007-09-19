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

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.Util;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Default implementation of {@link ICalculationUnit2D}
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public class CalculationUnit1D2D<ET extends IFE1D2DElement> extends CalculationUnit<ET> implements ICalculationUnit1D2D<ET>
{
  private IFeatureWrapperCollection<ICalculationUnit> m_subCalculationUnits;

  private IFeatureWrapperCollection<ET> m_elements;

  private List<ET> m_virtualElements;

  private final Feature m_featureToBind;

  private final QName m_qnameToBind;

  private final QName m_subUnitPropQName;

  public CalculationUnit1D2D( Feature featureToBind )
  {
    this( featureToBind, ICalculationUnit1D2D.QNAME, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS, Kalypso1D2DSchemaConstants.WB1D2D_PROP_CALC_UNIT, (Class<ET>) IFENetItem.class );
  }

  public CalculationUnit1D2D( Feature featureToBind, QName qnameToBind, QName elementListPropQName, QName subUnitPropQName, Class<ET> wrapperClass )
  {
    super( featureToBind, qnameToBind, elementListPropQName, wrapperClass );
    m_featureToBind = featureToBind;
    m_qnameToBind = qnameToBind;
    m_subUnitPropQName = subUnitPropQName;
    m_subCalculationUnits = Util.<ICalculationUnit> get( m_featureToBind, m_qnameToBind, m_subUnitPropQName, ICalculationUnit.class, true );
    m_elements = new FeatureWrapperCollection( m_featureToBind, IFE1D2DElement.class, Kalypso1D2DSchemaConstants.WB1D2D_PROP_ELEMENTS );
    ((FeatureWrapperCollection) m_elements).addSecondaryWrapper( IFELine.class );
    m_elements.clear();
    m_virtualElements = new ArrayList<ET>();
    for( final ICalculationUnit calculationUnit : m_subCalculationUnits )
      m_virtualElements.addAll( calculationUnit.getElements() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D#getSubUnits()
   */
  public IFeatureWrapperCollection<ICalculationUnit> getSubUnits( )
  {
    return m_subCalculationUnits;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit#getContinuityLines()
   */
  @Override
  public List<IFELine> getContinuityLines( )
  {
    final List<IFELine> lines = new ArrayList<IFELine>();
    for( final ICalculationUnit subUnit : m_subCalculationUnits )
    {
      final List<IFELine> continuityLines = subUnit.getContinuityLines();
      for( final IFELine line : continuityLines )
        if( !lines.contains( line ) )
          lines.add( line );
    }
    return lines;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit#contains(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  @Override
  public boolean contains( final IFeatureWrapper2 member )
  {
    if( member == null )
      return false;
    final String memberID = member.getGmlID();
    for( final IFeatureWrapper2 element : m_virtualElements )
      if( element.getGmlID().equals( memberID ) )
        return true;
    return false;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit#getElements1D()
   */
  @Override
  public List<IElement1D> getElements1D( )
  {
    final List<IElement1D> list = new ArrayList<IElement1D>();
    for( final IFeatureWrapper2 element : m_virtualElements )
      if( element instanceof IElement1D )
        list.add( (IElement1D) element );
    return list;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit#getElements2D()
   */
  @Override
  public List<IElement2D> getElements2D( )
  {
    final List<IElement2D> list = new ArrayList<IElement2D>();
    for( final IFeatureWrapper2 element : m_virtualElements )
      if( element instanceof IElement2D )
        list.add( (IElement2D) element );
    return list;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D#query(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  public List<IFENetItem> query( GM_Envelope envelope )
  {
    final List<IFENetItem> selectedElements = new ArrayList<IFENetItem>();
    for( final ICalculationUnit subUnit : m_subCalculationUnits )
      selectedElements.addAll( subUnit.getElements().query( envelope ) );
    return selectedElements;
  }
}
