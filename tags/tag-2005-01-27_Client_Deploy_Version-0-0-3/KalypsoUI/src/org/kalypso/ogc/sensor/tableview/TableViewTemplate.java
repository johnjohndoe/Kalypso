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
package org.kalypso.ogc.sensor.tableview;

import java.net.URL;
import java.util.Iterator;
import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.rules.ITableViewRules;
import org.kalypso.ogc.sensor.tableview.rules.RulesFactory;
import org.kalypso.ogc.sensor.template.AbstractViewTemplate;
import org.kalypso.template.obstableview.ObstableviewType;
import org.kalypso.template.obstableview.TypeObservation;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.ObstableviewType.RulesType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * A table view template for observations. Each observation is wrapped up in a
 * theme which in turn delivers columns for each value axis.
 * 
 * @author schlienger
 */
public class TableViewTemplate extends AbstractViewTemplate
{
  private final ITableViewRules m_rules = RulesFactory.getDefaultRules();

  public ITableViewRules getRules( )
  {
    return m_rules;
  }

  /**
   * Adds an observation and its values as columns to this template.
   * 
   * @param obs
   * @param args
   */
  public void addObservation( final IObservation obs,
      final IVariableArguments args )
  {
    final TableViewTheme theme = new TableViewTheme( this, null, args );

    // the theme should be created using the default properties of the obs
    theme.setUseDefault( true );
    // and following type should be ignored
    theme.setIgnoreType( getIgnoreType() );
    theme.setEditableColumns( false );

    addTheme( theme );

    theme.setObservation( obs );
  }

  /**
   * Sets the base template and loads the columns.
   * 
   * @param obsTableView
   * @param context
   */
  public void setBaseTemplate( final ObstableviewType obsTableView,
      final URL context )
  {
    removeAllThemes();
    
    final RulesType trules = obsTableView.getRules();
    if( trules != null )
    {
      // clear the rules since we get ones from the xml
      getRules().removeAllRules();
      
      for( final Iterator it = trules.getRenderingrule().iterator(); it
          .hasNext(); )
        getRules().addRule(
            RulesFactory.createRenderingRule( (TypeRenderingRule) it.next() ) );
    }

    final List list = obsTableView.getObservation();
    for( final Iterator it = list.iterator(); it.hasNext(); )
    {
      final TypeObservation tobs = (TypeObservation) it.next();

      final TableViewTheme theme = new TableViewTheme( this, null, tobs );
      addTheme( theme );

      final PoolableObjectType key = new PoolableObjectType(
          tobs.getLinktype(), tobs.getHref(), context );
      theme.loadObservation( key );
    }
  }

  /**
   * Convenienve method for adding an observation to this template.
   * 
   * @param themeName
   *          used as part of the col name if not null
   * @param context
   * @param href
   * @param linktype
   * @param ignoreExceptions
   * @param args
   * @return theme just being added
   */
  public TableViewTheme addObservation( final String themeName, final URL context,
      final String href, final String linktype, final boolean ignoreExceptions,
      final IVariableArguments args )
  {
    // create key according to observation link
    final PoolableObjectType key = new PoolableObjectType( linktype, href,
        context, ignoreExceptions );

    final TableViewTheme theme = new TableViewTheme( this, themeName, args );

    // the theme should be created using the default properties of the obs
    theme.setUseDefault( true );
    // and following type should be ignored
    theme.setIgnoreType( getIgnoreType() );

    addTheme( theme );

    theme.loadObservation( key );
    
    return theme;
  }
}