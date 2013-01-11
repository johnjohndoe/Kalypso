/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.imports.ctripple;

import java.net.URL;

import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.classifications.ICodeClass;
import org.kalypso.model.wspm.core.gml.classifications.IPartType;
import org.kalypso.model.wspm.core.gml.classifications.IStyleDefinition;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.tuhh.core.ctripple.CodedTrippleHorizonMapper;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Holger Abert
 */
public class CodedTrippleClassificationUpdater
{
  private final TuhhWspmProject m_targetProject;

  private final CodedTrippleImportData m_data;

  public CodedTrippleClassificationUpdater( final TuhhWspmProject targetProject, CodedTrippleImportData data )
  {
    m_targetProject = targetProject;
    m_data = data;
  }

  public void updateClassification( ) throws Exception
  {
    final IWspmClassification classification = m_targetProject.getClassificationMember();
    CodedTrippleHorizonMapper mapper = m_data.getCodedTrippleData().getMapper();

    updateCodeClassification( classification, mapper );
    updateStyleDefinitions( classification );
    updatePartTypes( classification, mapper );
  }

  private void updateCodeClassification( final IWspmClassification classification, CodedTrippleHorizonMapper mapper )
  {
    String[] codes = mapper.getCodes();
    for( final String code : codes )
    {
      final ICodeClass codeClass = classification.findCodeClass( code );
      if( codeClass == null )
      {
        final ICodeClass newCodeClass = classification.getCodeClassCollection().addNew( ICodeClass.FEATURE_CODE_CLASS );
        newCodeClass.setName( code );
        newCodeClass.setDescription( mapper.getCodeDescription( code ) );
        newCodeClass.setComment( mapper.getCodeDescription( code ) );
      }
    }
  }

  private void updateStyleDefinitions( final IWspmClassification classification ) throws Exception
  {
    final URL url = CodedTrippleClassificationUpdater.class.getResource( "resources/styleDefinitions.gml" );
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, null );
    final WspmProject rootFeature = (WspmProject)workspace.getRootFeature();
    final IWspmClassification wspmClassification = rootFeature.getClassificationMember();
    final IFeatureBindingCollection<IStyleDefinition> definitions = wspmClassification.getStyleDefinitionCollection();
    for( final IStyleDefinition definition : definitions )
    {
      final String style = definition.getName();
      final IStyleDefinition styleDefinition = classification.findStyleDefinition( style );
      if( styleDefinition == null )
        FeatureHelper.cloneFeature( classification, classification.getStyleDefinitionCollection().getFeatureList().getPropertyType(), definition );
    }
  }

  private void updatePartTypes( final IWspmClassification classification, CodedTrippleHorizonMapper mapper )
  {
    String[] horizonIds = mapper.getHorizonIds();
    for( final String horizonId : horizonIds )
    {
      final IPartType partType = classification.findPartType( horizonId );
      if( partType == null )
      {
        final IPartType newPartType = classification.getPartTypeCollection().addNew( IPartType.FEATURE_PART_TYPE );
        newPartType.setName( horizonId );
        newPartType.setDescription( mapper.getHorizonIdDescription( horizonId ) );
        newPartType.setComment( mapper.getHorizonIdDescription( horizonId ) );
        /* HINT: The style references should have the same id as the part here. */
        newPartType.setStyleReference( horizonId );
      }
    }
  }
}