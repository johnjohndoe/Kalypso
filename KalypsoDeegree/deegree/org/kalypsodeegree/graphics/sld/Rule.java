/*----------------    FILE HEADER  ------------------------------------------

This file is part of deegree.
Copyright (C) 2001 by:
EXSE, Department of Geography, University of Bonn
http://www.giub.uni-bonn.de/exse/
lat/lon Fitzke/Fretter/Poth GbR
http://www.lat-lon.de

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

Andreas Poth
lat/lon Fitzke/Fretter/Poth GbR
Meckenheimer Allee 176
53115 Bonn
Germany
E-Mail: poth@lat-lon.de

Jens Fitzke
Department of Geography
University of Bonn
Meckenheimer Allee 166
53115 Bonn
Germany
E-Mail: jens.fitzke@uni-bonn.de

 ---------------------------------------------------------------------------*/
package org.deegree.graphics.sld;

import org.deegree.services.wfs.filterencoding.*;

/**
 * A rule is used to attach a condition to and group the individual symbolizers
 * used for rendering. The Title and Abstract describe the rule and may be used
 * to generate a legend, as may the LegendGraphic. The Filter, ElseFilter,
 * MinScale, and MaxScale elements allow the selection of features and rendering
 * scales for a rule. The scale selection works as follows. When a map is to be
 * rendered, the scale denominator is computed and all rules in all UserStyles
 * that have a scale outside of the request range are dropped. (This also
 * includes Rules that have an ElseFilter.) An ElseFilter is simply an ELSE
 * condition to the conditions (Filters) of all other rules in the same UserStyle.
 * The exact meaning of the ElseFilter is determined after Rules have been
 * eliminated for not fitting the rendering scale. This definition of the
 * behaviour of ElseFilters may seem a little strange, but it allows for scale-
 * dependent and scale-independent ELSE conditions. For the Filter, only
 * SqlExpression is available for specification, but this is a hack and should
 * be replaced with Filter as defined in WFS. A missing Filter element means
 * "always true". If a set of Rules has no ElseFilters, then some features may
 * not be rendered (which is presumably the desired behavior). The Scales are
 * actually scale denominators (as double floats), so "10e6" would be interpreted
 * as 1:10M. A missing MinScale means there is no lower bound to the scale-
 * denominator range (lim[x-&gt;0+](x)), and a missing MaxScale means there is
 * no upper bound (infinity). 0.28mm
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 * @version $Revision$ $Date$
 */
public interface Rule {

   /**
    * returns the name of the rule. this for machine interpreting.
    * @return the name of the rule
    */
    String getName();

   /**
    * Sets the name attribute's value of the rule.
    * @param name the name of the rule
    * <p>
    */
    void setName (String name);    
    
   /**
    * returns the human readable title of the rule
    * @return the title of the rule
    */
    String getTitle();

   /**
    * Sets the title attribute's value of the rule.
    * @param title the title of the rule
    * <p>
    */
    void setTitle (String title);    
    
   /**
    * returns the human readable abstract of the rule
    * @return the abstract of the rule
    */
    String getAbstract();

   /**
    * Sets the abstract attribute's value of the Rule.
    * @param abstract_ the abstract of the rule
    */
    void setAbstract (String abstract_);    
    
   /**
    * The LegendGraphic element gives an optional explicit Graphic symbol to be
    * displayed in a legend for this rule.
    * @return the legendGraphic of the rule
    */
    LegendGraphic getLegendGraphic();

   /**
    * Sets the LegendGraphic element
    * @param legendGraphic the legendGraphic of the rule
    */
    void setLegendGraphic(LegendGraphic legendGraphic);    
    
   /**
    * The Filter element has a relatively straightforward meaning. The syntax of
    * the Filter element is defined in the WFS specification and allows both
    * attribute (property) and spatial filtering.
    * @return the filter element
    */
    Filter getFilter();
    
   /**
    * Sets the Filter.
    * @param filter the filter element
    */    
    void setFilter (Filter filter);

   /**
    * The ElseFilter allows rules to be specified that are activated for features
    * are not selected by any other rule in a feature-type style.
    * @return true if the rule has an elseFilter
    */
    boolean hasElseFilter();
    
   /**
    * sets the <ElseFilter>
    * @param elseFilter an elseFilter
    */
    public void setElseFilter( boolean elseFilter );
    
   /**
    * The MinScaleDenominator and MaxScaleDenominator elements of a Rule define
	* the range of map-rendering scales for which the rule should be applied.
    * The MinScaleDenominator and MaxScaleDenominator elements, as their names
	* suggest, are simply the minimum and maximum ranges of scale (denominators)
    * of maps for which a rule should apply.
    * @return the MinScaleDenominator for the rule
    */
    double getMinScaleDenominator();

   /**
    * Sets the MinScaleDenominator.
    * @param minScaleDenominator the MinScaleDenominator for the rule
    */
    void setMinScaleDenominator(double minScaleDenominator);    
    
   /**
    * The MinScaleDenominator and MaxScaleDenominator elements of a Rule define
	* the range of map-rendering scales for which the rule should be applied.
    * The MinScaleDenominator and MaxScaleDenominator elements, as their names
	* suggest, are simply the minimum and maximum ranges of scale (denominators)
    * of maps for which a rule should apply.
    * @return the MaxScaleDenominator for the rule
    */
    double getMaxScaleDenominator();

   /**
    * Sets the MaxScaleDenominator.
    * @param maxScaleDenominator the MaxScaleDenominator for the rule
    */
    void setMaxScaleDenominator(double maxScaleDenominator);     
    
   /**
    * Embedded inside of Rules, which group conditions for styling features, are
	* Symbolizers. A symbolizer describes how a feature is to appear on a map.
    * The symbolizer describes not just the shape that should appear but also
    * such graphical properties as color and opacity. A symbol is obtained by
    * specifying one of a small number of different types of symbolizer and then
    * supplying parameters to override its default behaviour. Currently, five
    * types of symbolizers are defined.<p></p>
    * The Symbolizers will be returned in the sequece of their occurence with in
    * the rule definition. Its the users function to determine what type of
    * Symbolizer(s) are returned. This can be done for example by using the
    * <tt>instanceof</tt> operator of Java.
    * @return the Symbolizer for the rule
    */
    Symbolizer[] getSymbolizers();

   /**
    * Sets a set of <Symbolizer>s.
    * @param symbolizers symbolizers for the rule
    */
    void setSymbolizers(Symbolizer[] symbolizers);

   /**
    * Adds a <Symbolizer> to a set of Symbolizers.
    * @param symbolizer symbolizer to add
    */
    void addSymbolizer(Symbolizer symbolizer);    
 
   /**
    * Removes a <Symbolizer> from a set of Symbolizers.
    * @param symbolizer symbolizer to remove
    */
    void removeSymbolizer(Symbolizer symbolizer);      
}
