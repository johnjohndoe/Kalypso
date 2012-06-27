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
package org.kalypso.ui.rrm.internal.results.view.tree.handlers;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.results.view.ResultManagementView;
import org.kalypso.ui.rrm.internal.results.view.base.IHydrologyResultReference;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Dirk Kuch
 */
public class HydrologyParameterSetUiHandler extends AbstractResultTreeNodeUiHandler
{

  private final Set<IHydrologyResultReference> m_references = Collections.synchronizedSet( new LinkedHashSet<IHydrologyResultReference>() );

  private final Feature m_feature;

  private final DESCRIPTORS m_existing;

  private final DESCRIPTORS m_missing;

  private final DESCRIPTORS m_invalid;

  public HydrologyParameterSetUiHandler( final RrmSimulation simulation, final RrmCalculationResult calculation, final Feature feature, final UIRrmImages.DESCRIPTORS existing, final UIRrmImages.DESCRIPTORS missing, final DESCRIPTORS invalid, final ResultManagementView view )
  {
    super( simulation, calculation, view );

    m_feature = feature;

    m_existing = existing;
    m_missing = missing;
    m_invalid = invalid;
  }

  public void addReferences( final IHydrologyResultReference... reference )
  {
    Collections.addAll( m_references, reference );
  }

  public IHydrologyResultReference[] getReferences( )
  {
    return m_references.toArray( new IHydrologyResultReference[] {} );
  }

  @Override
  public String getTreeLabel( )
  {
    final String name = m_feature.getName();
    final String description = m_feature.getDescription();

    if( StringUtils.isNotBlank( description ) )
      return String.format( "%s - %s", name, description ); //$NON-NLS-1$

    return name;
  }

  @Override
  protected String getTreeCompareLabel( )
  {
    final Feature feature = getFeature();

    if( feature instanceof Catchment )
    {
      return String.format( "AAA_%s", getTreeLabel() ); //$NON-NLS-1$
    }
    else if( feature instanceof Node )
    {
      return String.format( "BBB_%s", getTreeLabel() ); //$NON-NLS-1$
    }
    else if( feature instanceof StorageChannel )
    {
      return String.format( "CCC_%s", getTreeLabel() ); //$NON-NLS-1$
    }

    return super.getTreeCompareLabel();
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    return null;
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    final IHydrologyResultReference[] references = getReferences();
    for( final IHydrologyResultReference refernce : references )
    {
      if( refernce.isValid() && !refernce.isCalcualtionInput() )
        return UIRrmImages.id( m_existing );
    }

    if( isInvalid() )
      return UIRrmImages.id( m_invalid );

    return UIRrmImages.id( m_missing );
  }

  private boolean isInvalid( )
  {
    if( m_feature instanceof Catchment )
    {
      final Catchment catchment = (Catchment) m_feature;
      if( !catchment.isGenerateResults() )
        return true;
    }
    else if( m_feature instanceof Node )
    {
      final Node node = (Node) m_feature;
      if( !node.isGenerateResults() )
        return true;
    }
    else if( m_feature instanceof StorageChannel )
    {
      final StorageChannel channel = (StorageChannel) m_feature;
      if( !channel.isGenerateResults() )
        return true;
    }

    return false;
  }

  protected Feature getFeature( )
  {
    return m_feature;
  }

}
