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

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree_impl.model.feature.FeatureBindingCollection;

/**
 * Default implementation of {@link ICalculationUnit2D}
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public class CalculationUnit1D2D extends CoupledCalculationUnit implements ICalculationUnit1D2D
{
  public CalculationUnit1D2D( Object parent, IRelationType parentRelation, IFeatureType ft, String id, Object[] propValues )
  {
    super( parent, parentRelation, ft, id, propValues );
  }

  private final IFeatureBindingCollection<ICalculationUnit> m_subCalculationUnits = new FeatureBindingCollection<ICalculationUnit>( this, ICalculationUnit.class, WB1D2D_PROP_CALC_UNIT );

  private List<IFENetItem> m_virtualElements;

  private Set<String> m_virtualMemberIDs;

  private List<IPolyElement> m_list2DElements = null;

  private List<IElement1D> m_list1DElements = null;

  private void refreshVirtualElements( )
  {
    if( m_virtualElements != null )
    {
      int lIntCountAllElements = 0;
      for( final ICalculationUnit calculationUnit : m_subCalculationUnits )
      {
        lIntCountAllElements += calculationUnit.getElements().size();
      }
      if( m_virtualElements.size() == lIntCountAllElements )
      {
        return;
      }
    }
    m_virtualElements = new ArrayList<IFENetItem>();
    for( final ICalculationUnit calculationUnit : m_subCalculationUnits )
      m_virtualElements.addAll( calculationUnit.getElements() );

    calculate1DElements();
    calculate2DElements();
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D#getSubUnits()
   */
  @Override
  public IFeatureBindingCollection<ICalculationUnit> getChangedSubUnits( )
  {
    refreshVirtualElements();
    return m_subCalculationUnits;
  }

  private Set<String> getVirtualMemberIDs( )
  {
    if( m_virtualMemberIDs == null )
    {
      refreshVirtualElements();
      m_virtualMemberIDs = new HashSet<String>();
      for( final IFENetItem element : m_virtualElements )
        m_virtualMemberIDs.add( element.getId() );
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
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit#contains(org.kalypsodeegree.model.feature.binding.Feature)
   */
  @Override
  public boolean contains( final IFENetItem member )
  {
    if( member == null )
      return false;
    return getVirtualMemberIDs().contains( member.getId() );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit#getElements1D()
   */
  @Override
  public List<IElement1D> getElements1D( )
  {
    if( m_list1DElements == null )
    {
      calculate1DElements();
    }
    return m_list1DElements;
  }

  private void calculate1DElements( )
  {
    m_list1DElements = new ArrayList<IElement1D>();
    for( final IFENetItem element : m_virtualElements )
      if( element instanceof IElement1D )
        m_list1DElements.add( (IElement1D) element );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.discr.CalculationUnit#getElements2D()
   */
  @Override
  public List<IPolyElement> getElements2D( )
  {
    if( m_list2DElements == null )
    {
      calculate2DElements();
    }
    return m_list2DElements;
  }

  private void calculate2DElements( )
  {
    m_list2DElements = new ArrayList<IPolyElement>();
    for( final IFENetItem element : m_virtualElements )
      if( element instanceof IPolyElement )
        m_list2DElements.add( (IPolyElement) element );

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
