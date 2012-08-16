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

import java.util.Arrays;

import org.kalypso.model.wspm.core.gml.classifications.ICodeClass;
import org.kalypso.model.wspm.core.gml.classifications.IWspmClassification;
import org.kalypso.model.wspm.pdb.gaf.GafCode;
import org.kalypso.model.wspm.pdb.gaf.GafCodes;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class CheckoutCodeWorker
{
  private final CheckoutDataMapping m_mapping;

  private final IWspmClassification m_classification;

  private final GafCodes m_codes;

  public CheckoutCodeWorker( final CheckoutDataMapping mapping, final IWspmClassification classification, final GafCodes codes )
  {
    m_mapping = mapping;
    m_classification = classification;
    m_codes = codes;
  }

  public void execute( )
  {
    final IFeatureBindingCollection<ICodeClass> codeClassCollection = m_classification.getCodeClassCollection();

    final GafCode[] codes = m_codes.getAllCodes();

    /*
     * Sort by natual order of GAFCodes, i.e. by its number -> so codes will be written into WSPM classification in that
     * order.
     */
    Arrays.sort( codes );

    for( final GafCode code : codes )
    {
      final String name = code.getCode();
      final ICodeClass codeClass = m_classification.findCodeClass( name );
      if( codeClass == null )
        createCodeClass( code, codeClassCollection );
      else
        updateCodeClass( code, codeClass );
    }
  }

  private void createCodeClass( final GafCode code, final IFeatureBindingCollection<ICodeClass> collection )
  {
    final ICodeClass newClass = collection.addNew( ICodeClass.FEATURE_CODE_CLASS );
    newClass.setName( code.getCode() );

    updateProperties( code, newClass );

    m_mapping.addAddedFeatures( newClass );
  }

  private void updateCodeClass( final GafCode code, final ICodeClass codeClass )
  {
    final boolean changed = updateProperties( code, codeClass );
    if( changed )
      m_mapping.addChangedFeatures( codeClass );
  }

  private boolean updateProperties( final GafCode code, final ICodeClass newClass )
  {
    return new CheckoutCodeUpdater( code, newClass ).update();
  }

}