/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.gaja3d.service.client;

import java.io.FileInputStream;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.axis.message.MessageElement;
import org.apache.axis.message.addressing.EndpointReferenceType;
import org.apache.axis.types.URI.MalformedURIException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.globus.wsrf.NotificationConsumerManager;
import org.globus.wsrf.NotifyCallback;
import org.globus.wsrf.WSNConstants;
import org.globus.wsrf.core.notification.ResourcePropertyValueChangeNotificationElementType;
import org.globus.wsrf.encoding.ObjectDeserializer;
import org.globus.wsrf.encoding.SerializationException;
import org.kalypso.gaja3d.service.impl.Gaja3dQNames;
import org.kalypso.gaja3d.service.stubs.Gaja3DResourcePropertyType;
import org.oasis.wsn.NotificationProducer;
import org.oasis.wsn.Subscribe;
import org.oasis.wsn.TopicExpressionType;
import org.oasis.wsn.WSBaseNotificationServiceAddressingLocator;
import org.oasis.wsrf.properties.ResourcePropertyValueChangeNotificationType;
import org.oasis.wsrf.properties.ResourcePropertyValueChangeNotificationTypeOldValue;
import org.xml.sax.InputSource;

public class ValueListener implements NotifyCallback {

	private Log logger = LogFactory.getLog(ValueListener.class.getName());

	public static void main(String[] args) {
		ValueListener valueListener = new ValueListener();
		valueListener.run("test.epr");
	}

	public void run(String eprFilename) {
		try {
			System.setProperty("GLOBUS_LOCATION",
					"d:\\workspace3.4\\org.globus.ws.core");

			// start NotificationConsumer
			final NotificationConsumerManager consumer = NotificationConsumerManager
					.getInstance();
			consumer.startListening();
			final EndpointReferenceType consumerEPR = consumer
					.createNotificationConsumer(this);

			// get NotificationProducer
			final WSBaseNotificationServiceAddressingLocator notifLocator = new WSBaseNotificationServiceAddressingLocator();
			final FileInputStream fis = new FileInputStream(eprFilename);
			final EndpointReferenceType instanceEPR = (EndpointReferenceType) ObjectDeserializer
					.deserialize(new InputSource(fis),
							EndpointReferenceType.class);
			fis.close();
			final NotificationProducer producerPort = notifLocator
					.getNotificationProducerPort(instanceEPR);

			// call Subscribe
			producerPort.subscribe(buildRequest(consumerEPR,
					Gaja3dQNames.RP_BOUNDARY));
			producerPort.subscribe(buildRequest(consumerEPR,
					Gaja3dQNames.RP_BREAKLINES));
			producerPort.subscribe(buildRequest(consumerEPR,
					Gaja3dQNames.RP_DEM_GRID));
			logger.debug("Waiting for notification.");
			while (true) {
				try {
					Thread.sleep(30000);
				} catch (Exception e) {
					logger.debug("Interrupted while sleeping.");
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private Subscribe buildRequest(final EndpointReferenceType consumerEPR,
			final QName qName) throws MalformedURIException,
			SerializationException {
		final Subscribe request = new Subscribe();
		request.setUseNotify(true);
		request.setConsumerReference(consumerEPR);

		// GT 4.2
		// final SubscribeSubscriptionPolicy subscribeSubscriptionPolicy = new
		// SubscribeSubscriptionPolicy();
		// request.setSubscriptionPolicy(subscribeSubscriptionPolicy);

		final TopicExpressionType topicExpression = new TopicExpressionType();
		topicExpression.setDialect(WSNConstants.SIMPLE_TOPIC_DIALECT);
		topicExpression.setValue(qName);

		// GT 4.0
		request.setTopicExpression(topicExpression);

		// GT 4.2
		// final FilterType filterType = NotificationUtil
		// .createFilter(topicExpression);
		// request.setFilter(filterType);
		return request;
	}

	public synchronized void deliver(final List topicPath,
			final EndpointReferenceType producer, final Object message) {
		// GT 4.2
		// final NotificationMessageHolderTypeMessage notif_elem =
		// (NotificationMessageHolderTypeMessage) message;

		// GT 4.0
		final ResourcePropertyValueChangeNotificationElementType notif_elem = (ResourcePropertyValueChangeNotificationElementType) message;

		ResourcePropertyValueChangeNotificationType notif = null;

		// GT 4.2
		// try {
		// notif =
		// NotificationUtil.getRPValueChangeNotification(notif_elem);
		//
		// } catch (final DeserializationException e) {
		// e.printStackTrace();
		// }

		// GT 4.0
		notif = notif_elem.getResourcePropertyValueChangeNotification();

		logger.debug("A notification has been delivered");
		if (notif != null) {
			// GT 4.2
			// final MessageElement[] oldValues =
			// notif.getOldValues().get_any();
			// if (oldValues != null) {
			// for (final MessageElement messageElement : oldValues) {
			// try {
			// final String value = messageElement.getAsString();
			// logger.debug(String.format(
			// "Old value of RP %s: %s", messageElement
			// .getQName(), value));
			// } catch (final Exception e) {
			// e.printStackTrace();
			// }
			// }
			// }
			// final MessageElement[] newValues =
			// notif.getNewValues().get_any();
			// if (newValues != null) {
			// for (final MessageElement messageElement : newValues) {
			// try {
			// final String value = messageElement.getAsString();
			// logger.debug(String.format(
			// "New value of RP %s: %s", messageElement
			// .getQName(), value));
			// } catch (final Exception e) {
			// e.printStackTrace();
			// }
			// }
			// }

			// GT 4.0
			final ResourcePropertyValueChangeNotificationTypeOldValue oldValue = notif
					.getOldValue();
			if (oldValue != null) {
				final MessageElement[] anys = oldValue.get_any();
				if (anys != null && anys.length > 0) {
					final String value = anys[0].getValue();
					logger.debug("Old value: " + value);
				}
			}

			try {
				final MessageElement messageElement = notif.getNewValue()
						.get_any()[0];
				final Gaja3DResourcePropertyType rp = (Gaja3DResourcePropertyType) messageElement
						.getObjectValue(Gaja3DResourcePropertyType.class);
				final String value = messageElement.getAsString();
				logger.debug("New value: " + value);
			} catch (Exception e) {
				e.printStackTrace();
			}

		}
	}

}
