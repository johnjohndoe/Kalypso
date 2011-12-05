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
package org.kalypso.model.wspm.tuhh.schema.gml;

import java.net.URL;
import java.util.Properties;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.util.PropertiesUtilities;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree_impl.model.feature.GmlWorkspaceListener;
import org.kalypsodeegree_impl.model.feature.visitors.I18nFeatureVisitor;

/**
 * This listeners is used to i18n gml-dictionaries (like component definitions).
 *
 * @author thuel2
 */
public class DictI18nGmlListener extends GmlWorkspaceListener
{


  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#init(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  @Override
  public void init( final GMLWorkspace workspace )
  {
    final URL context = workspace.getContext();
    if( context == null )
      return;

    // determine, if we are interested in this gml file
    final String file = context.getFile();
    if( !file.startsWith( "/org/kalypso/model/wspm/tuhh/schema/dict/" ) )//$NON-NLS-1$
      return;

    // try to load properties
    final String filename = FileUtilities.nameFromPath( file );
    final String path = FileUtilities.nameWithoutExtension( filename );
    final Properties properties = new Properties();
    PropertiesUtilities.loadI18nProperties( properties, context, path );
    if( properties.isEmpty() )
      return;

    // replace i18n string
    final FeatureVisitor visitor = new I18nFeatureVisitor( properties );
    workspace.accept( visitor, workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IGmlWorkspaceListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  @Override
  public void onModellChange( final ModellEvent modellEvent )
  {
// nothing to do
  }

}
