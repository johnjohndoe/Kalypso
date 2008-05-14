/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.typehandler;

import java.net.URL;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gmlschema.types.AbstractOldFormatMarshallingTypeHandlerAdapter;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * ResourceFileTypeHandler
 * 
 * @author Nadja Peiler
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * <br>16.06.2005
 */
public class ResourceFileTypeHandler extends AbstractOldFormatMarshallingTypeHandlerAdapter
{
  public static final String NSHWS = "http://elbe.wb.tu-harburg.de/floodrisk/waterlevelData"; //$NON-NLS-1$

  public static final Class CLASSNAME = IFile.class;

  public static QName QNAME = new QName( NSHWS, "resourceFile" ); //$NON-NLS-1$

  public Class getValueClass( )
  {
    return CLASSNAME;
  }

  public QName getTypeName( )
  {
    return QNAME;
  }

  @Override
  public void marshall( Object object, Node node, URL context )
  {
    IFile resourceFile = (IFile) object;
    String path = resourceFile.getFullPath().toString();
    Document ownerDoc = node.getOwnerDocument();
    node.appendChild( ownerDoc.createTextNode( path ) );
  }

  @Override
  public Object unmarshall( Node node, URL context, IUrlResolver urlResolver ) 
  {
    String value = node.getFirstChild().getNodeValue();
    IPath path = new Path( value );
    return ResourceUtilities.findFileFromPath( path );
  }

  public String getShortname( )
  {
    return "IFile"; //$NON-NLS-1$
  }

  /**
   * @throws CloneNotSupportedException
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone, final String gmlVersion ) throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException( Messages.getString("org.kalypso.ogc.gml.typehandler.ResourceFileTypeHandler.3") ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    return false;
  }
}
