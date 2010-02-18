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
package org.kalypso.model.wspm.tuhh.ui.panel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.program.Program;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * @author Gernot Belger
 */
public class HyperlinkStyledText
{
  private final String m_text;

  private StyleRange m_styleRange;

  public HyperlinkStyledText( final String text )
  {
    m_text = text;
  }

  public StyledText createControl( final Composite parent, final int style )
  {
    final StyledText styledText = new StyledText( parent, style );
    styledText.setText( m_text );

    updateHyperLinkRanges( styledText );

    styledText.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        updateHyperLinkRanges( styledText );
      }
    } );

    styledText.addMouseMoveListener( new MouseMoveListener()
    {
      @Override
      public void mouseMove( final MouseEvent e )
      {
        final Display display = styledText.getDisplay();
        final StyleRange range = findStyleRange( styledText, e );
        if( range == null )
        {
          // Mouse left range or control: reset currentRange and cursor
          styledText.setCursor( display.getSystemCursor( SWT.CURSOR_ARROW ) );
          setCurrentRange( null );
        }
        else
          styledText.setCursor( display.getSystemCursor( SWT.CURSOR_HAND ) );
      }
    } );

    styledText.addMouseListener( new MouseAdapter()
    {
      @Override
      public void mouseDown( final MouseEvent e )
      {
        final StyleRange styleRange = findStyleRange( styledText, e );
        setCurrentRange( styleRange );
      }

      @Override
      public void mouseUp( final MouseEvent e )
      {
        hyperlinkClicked( styledText );
      }
    } );

    return styledText;
  }

  protected void hyperlinkClicked( final StyledText styledText )
  {
    if( m_styleRange == null )
      return;

    final String hyperlink = styledText.getTextRange( m_styleRange.start, m_styleRange.length );
    if( !Program.launch( hyperlink ) )
    {
      // Not necessary: windows shows an errror message of its own
// final String msg = String.format( "No programm found for: %s", hyperlink );
// MessageDialog.openError( styledText.getShell(), "Open Link", msg );
    }
  }

  protected void setCurrentRange( final StyleRange styleRange )
  {
    m_styleRange = styleRange;
  }

  protected void updateHyperLinkRanges( final StyledText styledText )
  {
    final String text = styledText.getText();
    final StyleRange[] hyperLinkRanges = findHyperLinkRanges( text );
    configureHyperLinkRanges( styledText, hyperLinkRanges );
    styledText.setStyleRanges( hyperLinkRanges );
  }

  private void configureHyperLinkRanges( final StyledText styledText, final StyleRange[] hyperLinkRanges )
  {
    for( final StyleRange styleRange : hyperLinkRanges )
    {
      styleRange.underline = true;
      styleRange.foreground = styledText.getDisplay().getSystemColor( SWT.COLOR_BLUE );
    }
  }

  private StyleRange[] findHyperLinkRanges( final String text )
  {
    final Collection<StyleRange> ranges = new ArrayList<StyleRange>();

    final Pattern pattern = Pattern.compile( " (http|ftp|https|file):/.*?( |$)" );
    final Matcher matcher = pattern.matcher( text );
    while( matcher.find() )
    {
      final boolean spaceEnd = " ".equals( matcher.group( 2 ) );

      final int start = matcher.start() + 1;
      final int end = matcher.end() - (spaceEnd ? 1 : 0);

      final StyleRange range = new StyleRange();
      range.start = start;
      range.length = end - start;
      ranges.add( range );
    }

    return ranges.toArray( new StyleRange[ranges.size()] );
  }

  StyleRange findStyleRange( final StyledText styledText, final MouseEvent e )
  {
    try
    {
      final int offsetAtLocation = styledText.getOffsetAtLocation( new Point( e.x, e.y ) );
      final StyleRange[] styleRanges = styledText.getStyleRanges();
      for( final StyleRange styleRange : styleRanges )
      {
        final int start = styleRange.start;
        final int end = start + styleRange.length - 1;
        if( start <= offsetAtLocation && offsetAtLocation <= end )
          return styleRange;
      }

      return styledText.getStyleRangeAtOffset( offsetAtLocation );
    }
    catch( final IllegalArgumentException ex )
    {
      return null;
    }
  }
}
