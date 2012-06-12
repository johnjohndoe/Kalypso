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
package org.kalypso.ui.rrm.internal.conversion.to12_02;

import java.io.File;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypsodeegree.xml.XMLTools;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Dirk Kuch
 */
public abstract class AbstractRemoveOsoleteXmlMembersWorker implements ICoreRunnableWithProgress
{

  private final File m_gmlFile;

  public AbstractRemoveOsoleteXmlMembersWorker( final File gmlFile )
  {
    m_gmlFile = gmlFile;
  }

  @Override
  public final IStatus execute( final IProgressMonitor monitor )
  {
    final StatusCollector collector = new StatusCollector( KalypsoUIRRMPlugin.getID() );

    try
    {
      final Document document = XMLTools.parse( m_gmlFile );

      final NodeList nodeList = document.getChildNodes();
      final Node[] remove = doIterate( nodeList );

      for( final Node r : remove )
      {
        document.removeChild( r );
      }

      doSave( document );

    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }

    return collector.asMultiStatus( String.format( "Removed obsolete members of gml file: %s.", m_gmlFile.getName() ) );
  }

  private void doSave( final Document document )
  {
    try
    {
      final TransformerFactory factory = TransformerFactory.newInstance();
      final Transformer transformer = factory.newTransformer();

      transformer.transform( new DOMSource( document ), new StreamResult( m_gmlFile ) );
      transformer.reset();
    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }
  }

  private Node[] doIterate( final NodeList nodeList )
  {
    final Set<Node> remove = new LinkedHashSet<>();

    for( int index = 0; index < nodeList.getLength(); index++ )
    {
      final Node node = nodeList.item( index );
      if( doInspect( node ) )
        remove.add( node );
    }

    return remove.toArray( new Node[] {} );
  }

  /**
   * @return delete node?
   */
  private boolean doInspect( final Node node )
  {
    final String localName = node.getLocalName();

    if( StringUtils.equalsIgnoreCase( getRootElementLocalName(), localName ) )
      doCleanNamespaces( node );

    final String[] properties = getInvalidProperties();
    for( final String property : properties )
    {
      if( StringUtils.equalsIgnoreCase( property, localName ) )
        return true;
    }

    final Node[] remove = doIterate( node.getChildNodes() );
    for( final Node r : remove )
    {
      node.removeChild( r );
    }

    return false;
  }

  protected abstract String[] getInvalidProperties( );

  protected abstract String getRootElementLocalName( );

  private void doCleanNamespaces( final Node node )
  {
    final Set<String> removals = new LinkedHashSet<>();

    final NamedNodeMap attributes = node.getAttributes();
    for( int index = 0; index < attributes.getLength(); index++ )
    {
      final Node item = attributes.item( index );
      final String textContent = item.getTextContent();

      final String[] namespaces = getInvalidNamespaces();
      for( final String namespace : namespaces )
      {
        if( StringUtils.equalsIgnoreCase( namespace, textContent ) ) //$NON-NLS-1$
          removals.add( item.getNodeName() );
      }
    }

    for( final String remove : removals )
    {
      attributes.removeNamedItem( remove );
    }
  }

  protected abstract String[] getInvalidNamespaces( );

}
