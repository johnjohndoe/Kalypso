@TypeDefs({ // 
@TypeDef(defaultForType = Geometry.class, typeClass = GeometryUserType2.class, parameters = { /*@Parameter(name = "dialect")*/}), //
  @TypeDef(defaultForType = com.vividsolutions.jts.geom.LineString.class, typeClass = GeometryUserType2.class, parameters = { /*@Parameter(name = "dialect")*/}), //
  @TypeDef(defaultForType = com.vividsolutions.jts.geom.Point.class, typeClass = GeometryUserType2.class, parameters = { /*@Parameter(name = "dialect")*/}) // 
})
package org.kalypso.model.wspm.pdb.db.mapping;

import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.TypeDefs;
import org.hibernatespatial.GeometryUserType2;

import com.vividsolutions.jts.geom.Geometry;

