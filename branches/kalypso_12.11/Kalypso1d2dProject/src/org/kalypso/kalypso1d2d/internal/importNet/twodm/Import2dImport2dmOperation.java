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
package org.kalypso.kalypso1d2d.internal.importNet.twodm;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypso1d2d.internal.importNet.AbstractImport2DImportOperation;
import org.kalypso.kalypso1d2d.internal.importNet.Import2dElementsData;
import org.kalypso.kalypso1d2d.internal.importNet.Import2dImportData;

import com.bce.gis.io.zweidm.IPolygonWithName;
import com.bce.gis.io.zweidm.ISmsModel;
import com.bce.gis.io.zweidm.SmsParser;

/**
 * @author Gernot Belger
 */
public class Import2dImport2dmOperation extends AbstractImport2DImportOperation
{
  private static final String EXTENSION_2DM = "*.2dm"; //$NON-NLS-1$

  public Import2dImport2dmOperation( final Import2dElementsData data, final Import2dImportData importData )
  {
    super( data, importData );
  }

  @Override
  public String getFilterName( )
  {
    return Messages.getString("Import2dImport2dmOperation_0"); //$NON-NLS-1$
  }

  @Override
  public String getFilterExtension( )
  {
    return EXTENSION_2DM;
  }

  @Override
  protected Pair<IStatus, IPolygonWithName[]> readFileData( final File importFile, final int sourceSrid, final IProgressMonitor monitor ) throws InvocationTargetException
  {
    final SmsParser parser = new SmsParser( sourceSrid );

    try
    {
      final URL url = importFile.toURI().toURL();
      final IStatus parseStatus = parser.parse( url, new NullProgressMonitor() );

      if( parseStatus.matches( IStatus.ERROR ) )
        return Pair.of( parseStatus, null );

      final ISmsModel model = parser.getModel();
      final SmsConverter converter = new SmsConverter( model );

      final SmsCollectorTarget target = new SmsCollectorTarget();
      converter.addTarget( target );

      converter.execute();

      final IPolygonWithName[] elements = target.getElements();

      return Pair.of( parseStatus, elements );
    }
    catch( final MalformedURLException e )
    {
      throw new InvocationTargetException( e );
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
  }
}