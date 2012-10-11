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

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.kalypso.model.wspm.core.gml.classifications.IStyle;
import org.kalypso.model.wspm.core.gml.classifications.IStyleDefinition;
import org.kalypso.model.wspm.core.gml.classifications.IStyleParameter;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.pdb.db.mapping.Style;
import org.kalypso.model.wspm.pdb.db.mapping.StyleArray;
import org.kalypso.model.wspm.pdb.db.mapping.StyleParameter;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Checks out and synchronizes pdb {@link org.kalypso.model.wspm.pdb.db.mapping.StyleArray} into the wspm definitions.
 *
 * @author Gernot Belger
 */
public class CheckoutStylesWorker
{
  private final CheckoutDataMapping m_mapping;

  private final IWspmClassification m_classification;

  private final StyleArray[] m_styleArrays;

  public CheckoutStylesWorker( final CheckoutDataMapping mapping, final IWspmClassification classification, final StyleArray[] styleArrays )
  {
    m_mapping = mapping;
    m_classification = classification;
    m_styleArrays = styleArrays;
  }

  public void execute( )
  {
    for( final StyleArray styleArray : m_styleArrays )
    {
      final String name = styleArray.getName();
      final IStyleDefinition styleDefinition = m_classification.findStyleDefinition( name );
      if( styleDefinition == null )
        createStyleDefinition( styleArray );
      else
        updateStyleDefinition( styleDefinition, styleArray );
    }
  }

  private void createStyleDefinition( final StyleArray styleArray )
  {
    final IFeatureBindingCollection<IStyleDefinition> styleDefinitions = m_classification.getStyleDefinitionCollection();

    final IStyleDefinition newDefinition = styleDefinitions.addNew( IStyleDefinition.FEATURE_STYLE_DEFINITION );
    newDefinition.setName( styleArray.getName() );

    updateStyleDefinition( newDefinition, styleArray );

    m_mapping.addAddedFeatures( newDefinition );
  }

  private void updateStyleDefinition( final IStyleDefinition styleDefinition, final StyleArray styleArray )
  {
    final boolean changed = updateProperties( styleDefinition, styleArray );
    if( changed )
      m_mapping.addChangedFeatures( styleDefinition );
  }

  private boolean updateProperties( final IStyleDefinition styleDefinition, final StyleArray styleArray )
  {
    /* reset old values */
    final IFeatureBindingCollection<IStyle> styleCollection = styleDefinition.getStyleCollection();
    styleCollection.clear();

    /* add new styles */
    final Set<Style> pdbStyles = styleArray.getStyles();

    /* sort by index */
    final Map<Long, Style> sortedPdbStyles = new TreeMap<>();
    for( final Style pdbStyle : pdbStyles )
      sortedPdbStyles.put( pdbStyle.getConsecutiveNum(), pdbStyle );

    /* create style elements */
    for( final Style pdbStyle : sortedPdbStyles.values() )
    {
      final IStyle newStyle = styleCollection.addNew( IStyle.FEATURE_STYLE );

      newStyle.setName( pdbStyle.getName() );
      newStyle.setDescription( pdbStyle.getDescription() );

      /* create parameters */
      final IFeatureBindingCollection<IStyleParameter> parameters = newStyle.getParameterCollection();
      final Set<StyleParameter> pdbParameters = pdbStyle.getStyleParameters();
      for( final StyleParameter pdbParameter : pdbParameters )
      {
        final IStyleParameter newParameter = parameters.addNew( IStyleParameter.FEATURE_STYLE_PARAMETER );

        newParameter.setKey( pdbParameter.getKey() );
        newParameter.setValue( pdbParameter.getValue() );
      }
    }

    // TODO we now always completely recreate the definition; would be more effective to only change really changed elements.
    return true;
  }
}