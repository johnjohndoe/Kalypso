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
package org.kalypso.model.wspm.tuhh.ui.light.internal;

import org.eclipse.ui.IViewSite;
import org.kalypso.ui.editor.gmleditor.part.GmvViewPart;

/**
 * Shows the local data of the PDB: a single fixed wspm project.
 * 
 * @author Gernot Belger
 */
public class WspmGmvViewPart extends GmvViewPart
{
  public static final String ID = "org.kalypso.model.wspm.tuhh.ui.light.internal.WspmGmvViewPart"; //$NON-NLS-1$

  @Override
  public void init( final IViewSite site )
  {
    super.init( site );

// PdbWspmUtils.ensureProject();
//
// final String inputContent = createInput();
//
// final StringStorage storage = new StringStorage( inputContent, null );
// storage.setName( "Local Data" );
//
// final IStorageEditorInput input = new StorageEditorInput( storage );
// setInput( input );
  }

// protected String createInput( )
// {
// try
// {
// final IFile modelFile = PdbWspmUtils.getModelFile();
// final URL modelURL = ResourceUtilities.createQuietURL( modelFile );
//
// final Gistreeview gistreeview = TemplateUtilities.OF_GISTREEVIEW.createGistreeview();
// final LayerType layerType = TemplateUtilities.OF_TEMPLATE_TYPES.createLayerType();
//
// layerType.setFeatureXPath( "id( 'root' )/wspm:waterBodyMember" ); // root feature
// layerType.setHref( modelURL.toExternalForm() );
//      layerType.setLinktype( "gml" ); //$NON-NLS-1$
//
// gistreeview.setInput( layerType );
//
// final Marshaller marshaller = TemplateUtilities.createGistreeviewMarshaller( CharEncoding.UTF_8 );
// final StringWriter sw = new StringWriter();
// marshaller.marshal( gistreeview, sw );
// sw.close();
// return sw.toString();
// }
// catch( final JAXBException e )
// {
// e.printStackTrace();
// return null;
// }
// catch( final IOException e )
// {
// e.printStackTrace();
// return null;
// }
// }
//
// /**
// * Forces a reload of the underlying data.<br/>
// * Any changes will be lost.
// */
// public void reload( )
// {
// final GmlTreeView viewer = getViewer();
// viewer.reload();
// }
//
// public TuhhWspmProject getProject( )
// {
// final GmlTreeView viewer = getViewer();
// final CommandableWorkspace workspace = viewer.getWorkspace();
// if( workspace == null )
// return null;
//
// return (TuhhWspmProject) workspace.getRootFeature();
// }
}