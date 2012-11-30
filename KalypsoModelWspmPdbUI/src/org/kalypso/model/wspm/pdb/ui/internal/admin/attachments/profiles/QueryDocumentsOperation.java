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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.profiles;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.hibernate.criterion.Restrictions;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Holger Albert
 */
public class QueryDocumentsOperation implements IPdbOperation
{
  private final String m_path;

  private Map<BigDecimal, List<Document>> m_result;

  public QueryDocumentsOperation( final String path )
  {
    m_path = path;
    m_result = null;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString("QueryDocumentsOperation_0"); //$NON-NLS-1$
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    try
    {
      final Criteria criteria = session.createCriteria( Document.class );
      criteria.add( Restrictions.like( Document.PROPERTY_FILENAME, String.format( "%s%%", m_path ) ) ); //$NON-NLS-1$
      final List<Document> dbDocuments = criteria.list();
      final Map<BigDecimal, List<Document>> dbHash = createDbHash( dbDocuments );
      m_result = dbHash;
    }
    catch( final HibernateException e )
    {
      throw new PdbConnectException( Messages.getString("QueryDocumentsOperation_2"), e ); //$NON-NLS-1$
    }
  }

  private Map<BigDecimal, List<Document>> createDbHash( final List<Document> dbDocuments )
  {
    final Map<BigDecimal, List<Document>> dbHash = new HashMap<>();

    for( final Document dbDocument : dbDocuments )
    {
      final CrossSection crossSection = dbDocument.getCrossSection();
      if( crossSection == null )
        continue;

      final BigDecimal station = crossSection.getStation();
      if( !dbHash.containsKey( station ) )
        dbHash.put( station, new ArrayList<Document>() );

      final List<Document> documents = dbHash.get( station );
      documents.add( dbDocument );
    }

    return dbHash;
  }

  public Map<BigDecimal, List<Document>> getResult( )
  {
    return m_result;
  }
}