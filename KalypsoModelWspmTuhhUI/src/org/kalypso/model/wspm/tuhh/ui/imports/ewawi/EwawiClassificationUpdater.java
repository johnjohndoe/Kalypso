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
package org.kalypso.model.wspm.tuhh.ui.imports.ewawi;

import org.kalypso.model.wspm.core.gml.classifications.ICodeClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.ewawi.data.enums.EwawiPunktart;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;

/**
 * @author Holger Abert
 */
public class EwawiClassificationUpdater
{
  private final TuhhWspmProject m_targetProject;

  public EwawiClassificationUpdater( final TuhhWspmProject targetProject )
  {
    m_targetProject = targetProject;
  }

  public void updateClassification( )
  {
    final IWspmClassification classification = m_targetProject.getClassificationMember();

    final EwawiPunktart[] values = EwawiPunktart.values();
    for( final EwawiPunktart value : values )
    {
      final String code = String.format( "EWAWI_%d", value.getKey() ); //$NON-NLS-1$
      final ICodeClass codeClass = classification.findCodeClass( code );
      if( codeClass == null )
      {
        final ICodeClass newCodeClass = classification.getCodeClassCollection().addNew( ICodeClass.FEATURE_CODE_CLASS );
        newCodeClass.setName( code );
        newCodeClass.setDescription( value.getLabel() );
        newCodeClass.setComment( value.getComment() );
      }
    }
  }
}