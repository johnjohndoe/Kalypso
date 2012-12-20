/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ui.rrm.internal.map.editRelation;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.command.AddLinkCommand;
import org.kalypso.ogc.gml.command.RemoveMemberCommand;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureLinkUtils;

/**
 * @author doemming
 */
public class LightRelationType implements IEditRelationType
{
  protected final IFeatureType m_sourceType;

  protected final IFeatureType m_targetType;

  private final IRelationType m_link;

  public LightRelationType( final IFeatureType sourceType, final IRelationType link, final IFeatureType targetType )
  {
    m_sourceType = sourceType;
    m_targetType = targetType;
    m_link = link;
  }

  @Override
  public String validate( final Feature source, final Feature target, final EditRelationMode mode )
  {
    final Feature[] existingLinks = source.resolveMembers( m_link );
    switch( mode )
    {
      case REMOVE:
        return validateRemove( target, existingLinks );

      case ADD:
        return validateAdd( target, existingLinks );
    }

    throw new IllegalArgumentException();
  }

  protected String validateAdd( final Feature target, final Feature[] existingLinks )
  {
    if( ArrayUtils.contains( existingLinks, target ) )
      return Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.HeavyRelationType.1" ); //$NON-NLS-1$

    final int maxOccurs = m_link.getMaxOccurs();
    if( existingLinks.length == 1 && maxOccurs == 1 )
      return Messages.getString( "org.kalypso.ui.rrm.internal.map.editRelation.LightRelationType.0" ); //$NON-NLS-1$

    if( existingLinks.length >= maxOccurs )
      return Messages.getString( "org.kalypso.ui.rrm.internal.map.editRelation.LightRelationType.1" ); //$NON-NLS-1$

    return null;
  }

  protected String validateRemove( final Feature target, final Feature[] existingLinks )
  {
    if( !ArrayUtils.contains( existingLinks, target ) )
      return Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.RelationType.0" ); //$NON-NLS-1$

    return null;
  }

  @Override
  public String toString( )
  {
    final Object sourceLabel = EditRelationUtils.getLabel( getSourceType() );
    final String linkLabel = EditRelationUtils.getLabel( getLink() );

    return String.format( "%s \u2192 %s", sourceLabel, linkLabel ); //$NON-NLS-1$
  }

  @Override
  public IFeatureType getTargetType( )
  {
    return m_targetType;
  }

  public IRelationType getLink( )
  {
    return m_link;
  }

  @Override
  public IFeatureType getSourceType( )
  {
    return m_sourceType;
  }

  @Override
  public ICommand getRemoveCommand( final Feature sourceFeature, final Feature targetFeature )
  {
    final Object member = FeatureLinkUtils.findMember( sourceFeature, getLink(), targetFeature );

    return new RemoveMemberCommand( sourceFeature, getLink(), member );
  }

  @Override
  public ICommand getAddCommand( final Shell shell, final Feature sourceFeature, final Feature targetFeature )
  {
    final IRelationType link = getLink();

    // REAMRK: make sure its the property of this feature type
    final IRelationType relation = (IRelationType) sourceFeature.getFeatureType().getProperty( link.getQName() );

    final String href = "#" + targetFeature.getId(); //$NON-NLS-1$

    return new AddLinkCommand( sourceFeature, relation, 0, href );
  }
}