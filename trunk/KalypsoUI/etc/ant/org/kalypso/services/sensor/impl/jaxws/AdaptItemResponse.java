
package org.kalypso.services.sensor.impl.jaxws;

import javax.xml.bind.annotation.AccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import org.kalypso.services.sensor.impl.jaxws.AdaptItemResponse;

@XmlRootElement(name = "adaptItemResponse", namespace = "http://impl.sensor.services.kalypso.org/")
@XmlAccessorType(AccessType.FIELD)
@XmlType(name = "adaptItemResponse", namespace = "http://impl.sensor.services.kalypso.org/")
public class AdaptItemResponse {

    @XmlElement(name = "return", namespace = "")
    private org.kalypso.services.sensor.ObservationBean _return;

    /**
     * 
     * @return
     *     returns ObservationBean
     */
    public org.kalypso.services.sensor.ObservationBean get_return() {
        return this._return;
    }

    /**
     * 
     * @param _return
     *     the value for the _return property
     */
    public void set_return(org.kalypso.services.sensor.ObservationBean _return) {
        this._return = _return;
    }

}
