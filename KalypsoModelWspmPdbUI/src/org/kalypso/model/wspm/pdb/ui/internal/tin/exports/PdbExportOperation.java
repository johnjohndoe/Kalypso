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
package org.kalypso.model.wspm.pdb.ui.internal.tin.exports;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IPath;
import org.hibernate.HibernateException;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Holger Albert
 */
public class PdbExportOperation implements IPdbOperation
{
  private final PdbExportConnectionChooserData m_settingsData;

  public PdbExportOperation( final PdbExportConnectionChooserData settingsData )
  {
    m_settingsData = settingsData;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString("PdbExportOperation_0"); //$NON-NLS-1$
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    try
    {
      final File sourceFile = m_settingsData.getSourceFile();
      final DhmIndex dhmIndex = m_settingsData.getDhmIndex();
      final IPath demServerPath = m_settingsData.getDemServerPath();
      final IPath targetPath = demServerPath.append( dhmIndex.getFilename() );

      session.saveOrUpdate( dhmIndex );

      FileUtils.copyFile( sourceFile, targetPath.toFile() );
    }
    catch( final HibernateException ex )
    {
      throw new PdbConnectException( Messages.getString("PdbExportOperation_1"), ex ); //$NON-NLS-1$
    }
    catch( final IOException ex )
    {
      throw new PdbConnectException( Messages.getString("PdbExportOperation_2"), ex ); //$NON-NLS-1$
    }
  }
}