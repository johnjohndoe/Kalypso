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

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ui.editor.gmleditor.command.AddRelationCommand;
import org.kalypso.ui.editor.gmleditor.command.RemoveRelationCommand;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author doemming
 */
public class LightRelationType implements IEditRelationType
{
  protected final IFeatureType m_srcFT;

  protected final IFeatureType m_destFT;

  private final IRelationType m_link;

  public LightRelationType( final IFeatureType srcFT, final IRelationType link, final IFeatureType destFT )
  {
    m_srcFT = srcFT;
    m_destFT = destFT;
    m_link = link;
  }

  @Override
  public String getFitProblems( final Feature f1, final Feature f2, final EditRelationMode mode )
  {
    final Feature[] existingLinks = f1.getWorkspace().resolveLinks( f1, m_link );

    switch( mode )
    {
      case REMOVE:
        if( ArrayUtils.contains( existingLinks, f2 ) )
          return null;
        else
          return Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.RelationType.0" );

      case ADD:
        if( ArrayUtils.contains( existingLinks, f2 ) )
          return Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.RelationType.1" ); //$NON-NLS-1$
        else
        {
          final int maxOccurs = m_link.getMaxOccurs();
          if( existingLinks.length >= maxOccurs )
            return "Maximum number of relations reached";
          else
            return null;
        }
    }

    throw new IllegalArgumentException();
  }

  @Override
  public String toString( )
  {
    return m_srcFT.getAnnotation().getLabel() + " > " + m_destFT.getAnnotation().getLabel(); //$NON-NLS-1$
  }

  @Override
  public IFeatureType getDestFT( )
  {
    return m_destFT;
  }

  public IRelationType getLink( )
  {
    return m_link;
  }

  @Override
  public IFeatureType getSrcFT( )
  {
    return m_srcFT;
  }

  @Override
  public ICommand getRemoveCommand( final Feature sourceFeature, final Feature targetFeature )
  {
    return new RemoveRelationCommand( sourceFeature, getLink(), targetFeature );
  }

  @Override
  public ICommand getAddCommand( final Feature sourceFeature, final Feature targetFeature )
  {
    return new AddRelationCommand( sourceFeature, getLink(), 0, targetFeature );
  }
}