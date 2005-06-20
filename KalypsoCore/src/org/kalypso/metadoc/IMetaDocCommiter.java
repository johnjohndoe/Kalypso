/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.metadoc;

import java.io.File;
import java.util.Map;
import java.util.Properties;

/**
 * IMetaDocCommiter. Commits documents with their meta information.
 * 
 * @author schlienger
 */
public interface IMetaDocCommiter
{
  public static final String KEY_AUTOR = "KEY_AUTOR";

  /**
   * Prepares the metadata with the required Metainformation.
   * 
   * @param serviceProps
   *          Properties of the MetaDoc service. Can be used to get additional properties relevant to the commiter
   * 
   * @param metadata
   * @throws MetaDocException
   */
  public void prepareMetainf( final Properties serviceProps, final Map metadata ) throws MetaDocException;

  /**
   * <p>
   * Commits the document described by the metadata.
   * </p>
   * <p>
   * Should delete document after commit operation
   * </p>
   * 
   * @param serviceProps
   *          Properties of the MetaDoc service. Can be used to get additional properties relevant to the commiter
   * @param metadata
   * @param doc
   * @throws MetaDocException
   */
  public void commitDocument( final Properties serviceProps, final Map metadata, final File doc )
      throws MetaDocException;
}