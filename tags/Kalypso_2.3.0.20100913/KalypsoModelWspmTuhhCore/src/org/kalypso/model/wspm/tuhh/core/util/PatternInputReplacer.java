/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.core.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;
import java.util.List;

import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.swt.widgets.Text;

/**
 * @author Gernot Belger
 */
public class PatternInputReplacer<T>
{
  private final List<IPatternInput<T>> m_replacers = new ArrayList<IPatternInput<T>>();

  public void addReplacer( final IPatternInput<T> replacer )
  {
    m_replacers.add( replacer );
  }

  public String getMessage( )
  {
    final Formatter formatter = new Formatter();

    for( final IPatternInput< ? extends T> token : m_replacers )
      formatter.format( "%s: %s%n", token.getToken(), token.getLabel() ); //$NON-NLS-1$

    return formatter.toString();
  }

  public String replaceTokens( final String text, final T context )
  {
    if( text == null )
      return null;

    final IPatternInput<T>[] tokens = getTokens();

    String result = text;
    for( final IPatternInput<T> token : tokens )
      result = token.replace( result, context );

    return result;
  }

  @SuppressWarnings("unchecked")
  private IPatternInput<T>[] getTokens( )
  {
    return m_replacers.toArray( new IPatternInput[m_replacers.size()] );
  }

  public IContributionItem[] asContributionItems( final Text text )
  {
    final Collection<IContributionItem> items = new ArrayList<IContributionItem>();

    for( final IPatternInput<T> pattern : m_replacers )
      items.add( new ActionContributionItem( new PatternAction( pattern, text ) ) );

    return items.toArray( new IContributionItem[items.size()] );
  }
}
