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
package org.kalypso.kalypsosimulationmodel.ui.calccore;

import java.io.File;
import java.net.MalformedURLException;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 * @author ig
 * 
 */
public class ChooseAdditionalFileControl extends AbstractFeatureControl implements IFeatureControl
{
  private File m_file;

  @SuppressWarnings("unused")
  private final File m_dir;

  private final String m_fileExt;

  public ChooseAdditionalFileControl( final Feature feature, final IPropertyType ftp, final File dir, final String fileExt )
  {
    super( feature, ftp );
    m_dir = dir;
    m_fileExt = fileExt;
    m_file = new File( (String) feature.getProperty( ftp ) );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  public Control createControl( final Composite parent, final int style )
  {
    final Button button = new Button( parent, style );
    button.setText( Messages.getString( "org.kalypso.ogc.gml.featureview.control.ChooseExeControl.0" ) ); //$NON-NLS-1$
    button.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleButtonPressed( e );
      }
    } );

    return button;
  }

  /**
   * Answer the source file location or <code>null</code> if unspecified
   */
  public IPath getSourceLocation( )
  {
    final String text = m_file == null ? "" : m_file.getAbsoluteFile().toString(); //$NON-NLS-1$
    if( text.length() == 0 )
      return null;
    final IPath path = new Path( text );
    if( !path.isAbsolute() )
      return ResourcesPlugin.getWorkspace().getRoot().getLocation().append( path );
    else
      return path;
  }

  /**
   * Open a file browser dialog to locate a source file
   */
  protected boolean browseForSourceFile( final SelectionEvent e )
  {
    final Shell shell = e.display.getActiveShell();
    IPath path = browse( shell, getSourceLocation() );
    if( path == null )
      return false;
    final IPath rootLoc = ResourcesPlugin.getWorkspace().getRoot().getLocation();
    if( rootLoc.isPrefixOf( path ) )
      path = path.setDevice( null ).removeFirstSegments( rootLoc.segmentCount() );
    m_file = new File( path.toString() );
    return true;
  }

  private IPath browse( final Shell shell, final IPath path )
  {
    final FileDialog dialog = new FileDialog( shell, SWT.OPEN );
    dialog.setFilterExtensions( new String[] { m_fileExt } );
    if( path != null )
    {
      if( path.segmentCount() > 1 )
        dialog.setFilterPath( path.removeLastSegments( 1 ).toOSString() );
      if( path.segmentCount() > 0 )
        dialog.setFileName( path.lastSegment() );
    }
    final String result = dialog.open();
    if( result == null )
      return null;
    return new Path( result );
  }

  protected void handleButtonPressed( final SelectionEvent e )
  {
    final boolean selectDone = browseForSourceFile( e );
    if( !selectDone )
    {
      return;
    }
    String newFile = null;
    try
    {
      newFile = m_file.toURI().toURL().toExternalForm();
    }
    catch( final MalformedURLException e1 )
    {
      e1.printStackTrace();
    }

    final ChangeFeatureCommand command = new ChangeFeatureCommand( getFeature(), getFeatureTypeProperty(), newFile );
    fireFeatureChange( command );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  @Override
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  @Override
  public void updateControl( )
  {
  }

}
