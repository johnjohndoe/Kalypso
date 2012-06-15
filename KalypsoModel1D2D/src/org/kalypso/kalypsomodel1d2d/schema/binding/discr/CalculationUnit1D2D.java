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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import org.kalypso.afgui.model.Util;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Default implementation of {@link ICalculationUnit2D}
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public class CalculationUnit1D2D extends CoupledCalculationUnit implements ICalculationUnit1D2D
{
  private final IFeatureWrapperCollection<ICalculationUnit> m_subCalculationUnits;

  private final List<IFENetItem> m_virtualElements;

  private Set<String> m_virtualMemberIDs;

  private final QName m_qnameToBind;

  private final QName m_subUnitPropQName;

  public CalculationUnit1D2D( final Feature featureToBind )
  {
    this( featureToBind, ICalculationUnit1D2D.QNAME, IFEDiscretisationModel1d2d.WB1D2D_PROP_ELEMENTS, ICalculationUnit1D2D.WB1D2D_PROP_CALC_UNIT, IFENetItem.class );
  }

  public CalculationUnit1D2D( final Feature featureToBind, final QName qnameToBind, final QName elementListPropQName, final QName subUnitPropQName, final Class<IFENetItem> wrapperClass )
  {
    super( featureToBind, qnameToBind, elementListPropQName, wrapperClass );
    m_qnameToBind = qnameToBind;
    m_subUnitPropQName = subUnitPropQName;
    m_subCalculationUnits = Util.get( featureToBind, m_qnameToBind, m_subUnitPropQName, ICalculationUnit.class, true );
    m_virtualElements = new ArrayList<IFENetItem>();
    for( final ICalculationUnit calculationUnit : m_subCalculationUnits )
      m_virtualElements.addAll( calculationUnit.getElements() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D#getSubUnits()
   */
  @Override
  public IFeatureWrapperCollection<ICalculationUnit> getChangedSubUnits( )
  {
    return m_subCalculationUnits;
  }

  private Set<String> getVirtualMemberIDs( )
  {
    if( m_virtualMemberIDs == null )
    {
      m_virtualMemberIDs = new HashSet<String>();
      for( final IFeatureWrapper2 element : m_virtualElements )
        m_virtualMemberIDs.add( element.getGmlID() );
    }
    return m_virtualMemberIDs;
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
  public boolean contains( final IFENetItem member )
  {
    if( member == null )
      return false;
    return getVirtualMemberIDs().contains( member.getGmlID() );
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
  public List<IPolyElement> getElements2D( )
  {
    final List<IPolyElement> list = new ArrayList<IPolyElement>();
    for( final IFeatureWrapper2 element : m_virtualElements )
      if( element instanceof IPolyElement )
        list.add( (IPolyElement) element );
    return list;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D#query(org.kalypsodeegree.model.geometry.GM_Envelope)
   */
  @Override
  public List<IFENetItem> query( final GM_Envelope envelope )
  {
    final List<IFENetItem> selectedElements = new ArrayList<IFENetItem>();
    for( final ICalculationUnit subUnit : m_subCalculationUnits )
      selectedElements.addAll( subUnit.getElements().query( envelope ) );
    return selectedElements;
  }
}
