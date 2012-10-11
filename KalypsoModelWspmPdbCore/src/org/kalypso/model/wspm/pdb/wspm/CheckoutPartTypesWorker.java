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
package org.kalypso.model.wspm.pdb.wspm;

import org.apache.commons.lang3.ObjectUtils;
import org.kalypso.model.wspm.core.gml.classifications.IPartType;
import org.kalypso.model.wspm.core.gml.classifications.IStyleDefinition;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPartType;
import org.kalypso.model.wspm.pdb.db.mapping.StyleArray;
import org.kalypso.model.wspm.pdb.db.utils.CrossSectionPartTypes;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Checks out and synchronizes pdb {@link org.kalypso.model.wspm.pdb.db.mapping.StyleArray} into the wspm definitions.
 *
 * @author Gernot Belger
 */
public class CheckoutPartTypesWorker
{
  private final CheckoutDataMapping m_mapping;

  private final IWspmClassification m_classification;

  private final CrossSectionPartTypes m_partTypes;

  public CheckoutPartTypesWorker( final CheckoutDataMapping mapping, final IWspmClassification classification, final CrossSectionPartTypes partTypes )
  {
    m_mapping = mapping;
    m_classification = classification;
    m_partTypes = partTypes;
  }

  public void execute( )
  {
    final CrossSectionPartType[] pdbTypes = m_partTypes.getTypes();
    for( final CrossSectionPartType pdbPartType : pdbTypes )
    {
      final String category = pdbPartType.getCategory();

      final IPartType wspmPartType = m_classification.findPartType( category );
      if( wspmPartType == null )
        createPartType( pdbPartType );
      else
        updatePartType( wspmPartType, pdbPartType );
    }
  }

  private void createPartType( final CrossSectionPartType pdbPartType )
  {
    final IFeatureBindingCollection<IPartType> wspmPartTypes = m_classification.getPartTypeCollection();

    final IPartType newType = wspmPartTypes.addNew( IPartType.FEATURE_PART_TYPE );

    final String category = pdbPartType.getCategory();

    // FIXME: map category to wspm part type id

    newType.setName( category );

    updatePartType( newType, pdbPartType );

    newType.setComment( String.format( "Create from PDB type '%s'", category ) );

    m_mapping.addAddedFeatures( newType );
  }

  private void updatePartType( final IPartType wspmPartType, final CrossSectionPartType pdbPartType )
  {
    final boolean changed = updateProperties( wspmPartType, pdbPartType );
    if( changed )
      m_mapping.addChangedFeatures( wspmPartType );
  }

  private boolean updateProperties( final IPartType wspmPartType, final CrossSectionPartType pdbPartType )
  {
    boolean changed = false;

    changed |= updateDescription( wspmPartType, pdbPartType );
    changed |= updateStyleReference( wspmPartType, pdbPartType );

    if( changed )
    {
      final String comment = String.format( "Updated from PDB type '%s'", pdbPartType.getCategory() );
      wspmPartType.setComment( comment );
    }

    return changed;
  }

  private boolean updateDescription( final IPartType wspmPartType, final CrossSectionPartType pdbPartType )
  {
    final String newDescription = pdbPartType.getDescription();
    final String oldDescription = wspmPartType.getDescription();
    if( ObjectUtils.equals( newDescription, oldDescription ) )
      return false;

    wspmPartType.setDescription( newDescription );
    return true;
  }

  private boolean updateStyleReference( final IPartType wspmPartType, final CrossSectionPartType pdbPartType )
  {
    final StyleArray styleArray = pdbPartType.getStyleArray();
    final String oldName = styleArray == null ? null : styleArray.getName();

    final IStyleDefinition styleDefinition = wspmPartType.getStyleDefinition();
    final String newName = styleDefinition == null ? null : styleDefinition.getName();

    if( ObjectUtils.equals( newName, oldName ) )
      return false;

    wspmPartType.setStyleReference( newName );
    return true;
  }
}