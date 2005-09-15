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
import java.text.ParseException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.extension.TypeRegistryException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * ResourceFileTypeHandler
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (16.06.2005)
 */
public class ResourceFileTypeHandler implements IMarshallingTypeHandler
{
  public static final String NSHWS = "http://elbe.wb.tu-harburg.de/floodrisk/waterlevelData";

  public static final String TYPENAME = NSHWS + ":" + "resourceFile";

  public static final String CLASSNAME = IFile.class.getName();

  public String getClassName()
  {
    return CLASSNAME;
  }

  public String getTypeName()
  {
    return TYPENAME;
  }

  public void marshall( Object object, Node node, URL context ) throws TypeRegistryException
  {
    IFile resourceFile = (IFile)object;
    String path = resourceFile.getFullPath().toString();
    Document ownerDoc = node.getOwnerDocument();
    node.appendChild( ownerDoc.createTextNode( path ) );
  }

  public Object unmarshall( Node node, URL context, IUrlResolver urlResolver ) throws TypeRegistryException
  {
    String value = node.getFirstChild().getNodeValue();
    IPath path = new Path( value );
    return ResourceUtilities.findFileFromPath( path );
  }

  public String getShortname()
  {
    return "IFile";
  }

  /**
   * @throws CloneNotSupportedException
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone ) throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException( "Not clonable!" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

}
