
package org.kalypso.services.sensor.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.sensor.impl.jaxws.ReadDataResponse;

@XmlRootElement(name = "readDataResponse", namespace = "http://impl.sensor.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "readDataResponse", namespace = "http://impl.sensor.services.kalypso.org/")
public class ReadDataResponse {

    @XmlElement(name = "return", namespace = "")
    private org.kalypso.services.sensor.DataBean _return;

    /**
     * 
     * @return
     *     returns DataBean
     */
    public org.kalypso.services.sensor.DataBean get_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(org.kalypso.services.sensor.DataBean _return) {
        this._return = _return;
    }

}
